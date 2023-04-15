with Ada.Calendar;
with Ada.Streams;
with Ada.Strings.Unbounded;

with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Server.Push;

package WS_CB is

   use Ada.Calendar;
   use Ada.Streams;
   use Ada.Strings.Unbounded;

   WS : AWS.Server.HTTP;

   procedure Stop_Push_Server;

   function Service (Request : AWS.Status.Data) return AWS.Response.Data;
   --  Simple ID generator

   protected New_Client_Id is
      procedure Get (New_Id : out String);
   private
      Id : Natural := 0;
   end New_Client_Id;

   type Client_Env is record
      Start   : Time;
      Picture : Unbounded_String;
   end record;

   function To_Array
     (Time : Ada.Calendar.Time;
      Env  : Client_Env) return Stream_Element_Array;

   package Time_Push is new AWS.Server.Push
     (Client_Output_Type => Ada.Calendar.Time,
      Client_Environment => Client_Env,
      To_Stream_Array    => To_Array);

   SP : Time_Push.Object;

end WS_CB;