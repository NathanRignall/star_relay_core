with Ada.Environment_Variables;

with Hardware;
with Drivers.Ethernet;

package body Application is

   procedure Initialize is

      -- load the environment variables
      Device_Identifier_Env : constant String :=
        Ada.Environment_Variables.Value ("DEVICE_IDENTIFIER");

      -- create the device identifier
      Device_Identifier : constant Library.Network.Device_Identifier_Type :=
        Library.Network.Device_Identifier_Type'Value (Device_Identifier_Env);

      -- create the addresses using the environment variables
      Radio_Multicast_Address : constant Drivers.Ethernet.Address_V4_Type :=
        Drivers.Ethernet.Address_V4_Type'(127, 0, 0, 1);

      Cloud_Server_Address : constant Drivers.Ethernet.Address_V4_Type :=
        Drivers.Ethernet.Address_V4_Type'(127, 0, 0, 1);

   begin

      -- create the applications
      Network   :=
        new Library.Network.Network_Type
          (Device_Identifier => Device_Identifier, Radio => Hardware.Radio,
           Cloud             => Hardware.Cloud);
      Telemetry := new Library.Telemetry.Telemetry_Type (Network => Network);

      -- initialize the applications
      Network.Initialize
        (Radio_Multicast_Address => Radio_Multicast_Address,
         Cloud_Server_Address    => Cloud_Server_Address);
      Telemetry.Initialize;

   end Initialize;

   procedure Schedule (Cycle : Types.Schedule.Cycle_Type) is
   begin

      -- schedule the applications
      Network.Schedule (Cycle);
      Telemetry.Schedule (Cycle);

   end Schedule;

end Application;
