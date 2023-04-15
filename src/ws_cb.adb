------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Real_Time;

with GNAT.Calendar.Time_IO;
with GNATCOLL.JSON;

with AWS.Config;
with AWS.Messages;
with AWS.MIME;
with AWS.Net;
with AWS.Parameters;
with AWS.Services.Directory;

with AWS.Translator;
with AWS.Utils;

with Application.State;

with Types.State;
with Types.Physics;

package body WS_CB is

   use Ada;
   use AWS;

   task Server_Push_Task;

   function Get (Request : AWS.Status.Data) return AWS.Response.Data;
   function Put (Request : AWS.Status.Data) return AWS.Response.Data;

   function Service (Request : AWS.Status.Data) return AWS.Response.Data is
      use type AWS.Status.Request_Method;
   begin
      if AWS.Status.Method (Request) = AWS.Status.GET
        or else AWS.Status.Method (Request) = AWS.Status.POST
        or else AWS.Status.Method (Request) = AWS.Status.HEAD
      then
         return Get (Request);

      elsif AWS.Status.Method (Request) = AWS.Status.PUT then
         return Put (Request);

      else
         return AWS.Response.Acknowledge (Status_Code => Messages.S405);
      end if;

   exception
      when E : others =>
         return
           AWS.Response.Build
             (Content_Type => "text/plain", Status_Code => AWS.Messages.S500,
              Message_Body => Ada.Exceptions.Exception_Information (E));
   end Service;

   function JSON_Test return String is
      My_Obj   : GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Create_Object;
      My_Array : GNATCOLL.JSON.JSON_Array := GNATCOLL.JSON.Empty_Array;

      use type Ada.Real_Time.Time_Span;
   begin
      My_Obj.Set_Field ("test", "hello world");

      -- loop over aircraft state and add to array
      for Aircraft_Index in Types.State.Aircraft_Index_Type loop

         declare
            Aircraft : Types.State.Aircraft_Type :=
              Application.State.Aircraft_State (Aircraft_Index);

            Aircraft_Obj : GNATCOLL.JSON.JSON_Value :=
              GNATCOLL.JSON.Create_Object;
            Position_Obj : GNATCOLL.JSON.JSON_Value :=
              GNATCOLL.JSON.Create_Object;
            Velocity_Obj : GNATCOLL.JSON.JSON_Value :=
              GNATCOLL.JSON.Create_Object;

            Time_Since_Update          : Ada.Real_Time.Time_Span :=
              Ada.Real_Time.Clock - Aircraft.Time;
            Time_Since_Update_Duration : Duration                :=
              Ada.Real_Time.To_Duration (Time_Since_Update);
         begin

            Position_Obj.Set_Field ("lat", Float (Aircraft.Position.Latitude));
            Position_Obj.Set_Field
              ("lon", Float (Aircraft.Position.Longitude));
            Position_Obj.Set_Field ("alt", Float (Aircraft.Position.Altitude));

            Velocity_Obj.Set_Field
              ("x", Float (Aircraft.Velocity_Vector (Types.Physics.X)));
            Velocity_Obj.Set_Field
              ("y", Float (Aircraft.Velocity_Vector (Types.Physics.Y)));
            Velocity_Obj.Set_Field
              ("z", Float (Aircraft.Velocity_Vector (Types.Physics.Z)));

            Aircraft_Obj.Set_Field ("id", Integer (Aircraft.Identifier));
            Aircraft_Obj.Set_Field ("position", Position_Obj);
            Aircraft_Obj.Set_Field ("velocity", Velocity_Obj);
            Aircraft_Obj.Set_Field
              ("time", Float (Time_Since_Update_Duration));

            if Aircraft.Active then
               GNATCOLL.JSON.Append (My_Array, Aircraft_Obj);
            end if;

         end;

      end loop;

      My_Obj.Set_Field ("aircraft", My_Array);

      return My_Obj.Write;
   end JSON_Test;

   function Get (Request : AWS.Status.Data) return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);
   begin
      if URI = "/ref" then
         return
           AWS.Response.Moved
             (Location => "http://localhost:1234/demos/page1.html");

      elsif URI = "/json" then
         return
           AWS.Response.Acknowledge
             (Status_Code  => AWS.Messages.S200,
              Content_Type => "application/json", Message_Body => JSON_Test);

      elsif URI = "/server_push" then

         declare
            P_List : constant AWS.Parameters.List :=
              AWS.Status.Parameters (Request);

            Picture : Unbounded_String :=
              To_Unbounded_String (AWS.Parameters.Get_Value (P_List));

            Client_Id : String (1 .. 32);
         begin
            New_Client_Id.Get (Client_Id);

            if Picture = Null_Unbounded_String then
               Picture := To_Unbounded_String ("%D - %T");
            end if;

            Time_Push.Register
              (Server            => SP, Client_Id => Client_Id,
               Socket => AWS.Net.Socket_Access'(AWS.Status.Socket (Request)),
               Environment       => (Clock, Picture),
               Init_Content_Type => "text/plain",
               Init_Data => Ada.Calendar.Clock, Kind => Time_Push.Multipart);
         end;

         return AWS.Response.Socket_Taken;

      else
         return
           AWS.Response.Acknowledge
             (Messages.S404, "<p>Page '" & URI & "' Not found.");
      end if;
   end Get;

   function Put (Request : AWS.Status.Data) return AWS.Response.Data is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Acknowledge (Status_Code => AWS.Messages.S200);
   end Put;

   ----------------------
   -- Stop_Push_Server --
   ----------------------

   procedure Stop_Push_Server is
   begin
      abort Server_Push_Task;
   end Stop_Push_Server;

   --------------
   -- To_Array --
   --------------

   function To_Array
     (Time : Ada.Calendar.Time; Env : Client_Env)
      return Ada.Streams.Stream_Element_Array
   is
      use GNAT.Calendar.Time_IO;
   begin
      return
        Translator.To_Stream_Element_Array
          (Image (Time, Picture_String (To_String (Env.Picture))) & ASCII.CR &
           ASCII.LF & Duration'Image (Time - Env.Start));
   end To_Array;

   ----------------------
   -- Server_Push_Task --
   ----------------------

   task body Server_Push_Task is
      Tm : Calendar.Time;
   begin
      loop
         begin
            delay 1.0;
            Tm := Ada.Calendar.Clock;

            Text_IO.Put_Line
              ("Send time " & GNAT.Calendar.Time_IO.Image (Tm, "%D - %T"));

            Time_Push.Send (SP, Tm, Content_Type => "text/plain");

         exception
            when E : others =>
               Text_IO.Put_Line ("Server_Push_Task error");
               Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         end;
      end loop;
   end Server_Push_Task;

   -------------------
   -- New_Client_ID --
   -------------------

   protected body New_Client_Id is

      procedure Get (New_Id : out String) is
      begin
         Id := Id + 1;
         Integer_Text_IO.Put (New_Id, Id);
      end Get;

   end New_Client_Id;

end WS_CB;
