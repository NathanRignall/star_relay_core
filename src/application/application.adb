with Ada.Environment_Variables;

with Hardware;

package body Application is

   procedure Initialize is

      -- load the environment variables
      Device_Identifier_Env : constant String :=
        Ada.Environment_Variables.Value ("DEVICE_IDENTIFIER");

      -- create the device identifier
      Device_Identifier : constant Library.Network.Device_Identifier_Type :=
        Library.Network.Device_Identifier_Type'Value (Device_Identifier_Env);

   begin

      Network :=
        new Library.Network.Network_Type
          (Device_Identifier => Device_Identifier, Radio => Hardware.Radio,
           Cloud             => Hardware.Cloud);
      Network.Initialize;

      Telemetry := new Library.Telemetry.Telemetry_Type (Network => Network);
      Telemetry.Initialize;

   end Initialize;

   procedure Schedule (Cycle : Types.Schedule.Cycle_Type) is
   begin

      Network.Schedule (Cycle);

      Telemetry.Schedule (Cycle);

   end Schedule;

end Application;
