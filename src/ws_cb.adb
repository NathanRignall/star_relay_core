with Ada.Exceptions;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Real_Time;

with GNAT.Calendar.Time_IO;
with GNATCOLL.JSON;

with AWS.Config;
with AWS.Messages;
with AWS.MIME;
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

   function JSON_Aircraft return String is

      use type Ada.Real_Time.Time_Span;

      Main_Object   : GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Create_Object;
      Aircraft_Array : GNATCOLL.JSON.JSON_Array := GNATCOLL.JSON.Empty_Array;

   begin

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
            Aircraft_Obj.Set_Field ("active", Aircraft.Active);
            Aircraft_Obj.Set_Field ("position", Position_Obj);
            Aircraft_Obj.Set_Field ("velocity", Velocity_Obj);
            Aircraft_Obj.Set_Field
              ("time", Float (Time_Since_Update_Duration));

            GNATCOLL.JSON.Append (Aircraft_Array, Aircraft_Obj);

         end;

      end loop;

      Main_Object.Set_Field ("aircraft", Aircraft_Array);

      return Main_Object.Write;
   end JSON_Aircraft;

   function Get (Request : AWS.Status.Data) return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);
   begin
      if URI = "/json" then
         return
           AWS.Response.Acknowledge
             (Status_Code  => AWS.Messages.S200,
              Content_Type => "application/json", Message_Body => JSON_Aircraft);
      else
         return AWS.Response.Acknowledge (Messages.S404, URI & " - Not found");
      end if;
   end Get;

   function Put (Request : AWS.Status.Data) return AWS.Response.Data is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Acknowledge (Status_Code => AWS.Messages.S200);
   end Put;

end WS_CB;
