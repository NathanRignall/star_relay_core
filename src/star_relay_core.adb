with Ada.Text_IO;

with AWS.Server;
with AWS.Config.Set;

with WS_CB;

with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Text_IO;

with Hardware;
with Application;

with Types.Physics;
with Types.Schedule;

procedure Star_Relay_Core is

   WS     : AWS.Server.HTTP;
   Config : AWS.Config.Object;

      use type Ada.Real_Time.Time;

   task Schedule;

   procedure Dispatch_Schedule (Cycle : Types.Schedule.Cycle_Type) is
   begin

      Hardware.Schedule (Cycle);
      Application.Schedule (Cycle);

   end Dispatch_Schedule;

   task body Schedule is
      Cycle_Delay : constant Ada.Real_Time.Time_Span :=
        Ada.Real_Time.Milliseconds (20);

      Start_Delay : constant Ada.Real_Time.Time_Span :=
        Ada.Real_Time.Milliseconds (3_000);
      Next        : Ada.Real_Time.Time := Ada.Real_Time.Clock + Start_Delay;

      type Cycle_Count is range 1 .. 250;
      Cycle : Cycle_Count := Cycle_Count'First;

   begin

      Ada.Text_IO.Put_Line ("Starting Schedule");

      Hardware.Initialize;
      Application.Initialize;

      loop
         delay until Next;

         -- run 20ms cycle
         Dispatch_Schedule (Types.Schedule.S_20ms);

         -- chek if 50ms cycle is due
         if Cycle mod 2 = 0 then
            Dispatch_Schedule (Types.Schedule.S_50ms);
         end if;

         -- check if 100ms cycle is due
         if Cycle mod 5 = 0 then
            Dispatch_Schedule (Types.Schedule.S_100ms);
         end if;

         -- check if 200ms cycle is due
         if Cycle mod 10 = 0 then
            Dispatch_Schedule (Types.Schedule.S_200ms);
         end if;

         -- check if 500ms cycle is due
         if Cycle mod 25 = 0 then
            Dispatch_Schedule (Types.Schedule.S_500ms);
         end if;

         -- check if 1s cycle is due
         if Cycle mod 50 = 0 then
            Dispatch_Schedule (Types.Schedule.S_1000ms);
         end if;

         -- check if 2s cycle is due
         if Cycle mod 100 = 0 then
            Dispatch_Schedule (Types.Schedule.S_2000ms);
         end if;

         -- check if 5s cycle is due
         if Cycle mod 250 = 0 then
            Dispatch_Schedule (Types.Schedule.S_5000ms);
         end if;

         -- update cycle counter if not at end
         if Cycle < Cycle_Count'Last then
            Cycle := Cycle + 1;
         else
            Cycle := Cycle_Count'First;
         end if;

         -- update next time
         Next := Next + Cycle_Delay;

         --  if Next < Ada.Real_Time.Clock then
         --     Ada.Text_IO.Put_Line ("Schedule overrun in cycle counter " & Cycle'Image);
         --  end if;

      end loop;

   exception

      when Exep : others =>
         Ada.Text_IO.Put_Line ("Exception");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message(Exep));

   end Schedule;

begin
   Ada.Text_IO.Put_Line ("Ada Web Server " & AWS.Version);

   AWS.Config.Set.Reuse_Address (Config, True);
   AWS.Config.Set.Server_Host (Config, "0.0.0.0");
   AWS.Config.Set.Server_Port (Config, 1_234);
   AWS.Config.Set.Server_Name (Config, "WS Demo");
   AWS.Config.Set.Max_Connection (Config, 5);

   AWS.Server.Start
     (WS_CB.WS, Config => Config, Callback => WS_CB.Service'Access);

   delay 10 * 60.0;

end Star_Relay_Core;
