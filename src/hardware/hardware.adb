with Ada.Environment_Variables;
with Ada.Text_IO;

package body Hardware is

   procedure Initialize is

      -- load the environment variables
      Cloud_Multicast_Address_Env : constant String :=
        Ada.Environment_Variables.Value ("CLOUD_MULTICAST_ADDRESS");

      -- create the addresses
      Cloud_Address : constant Drivers.Ethernet.Address_V4_Access_Type :=
        new Drivers.Ethernet.Address_V4_Type'(0, 0, 0, 0);

      Cloud_Multicast_Address :
        constant Drivers.Ethernet.Address_V4_Access_Type :=
        new Drivers.Ethernet.Address_V4_Type'
          (Drivers.Ethernet.Address_V4_Type'
             (1 =>
                Drivers.Ethernet.Address_Octet_Type'Value
                  (Cloud_Multicast_Address_Env (1 .. 3)),
              2 =>
                Drivers.Ethernet.Address_Octet_Type'Value
                  (Cloud_Multicast_Address_Env (5 .. 7)),
              3 =>
                Drivers.Ethernet.Address_Octet_Type'Value
                  (Cloud_Multicast_Address_Env (9 .. 11)),
              4 =>
                Drivers.Ethernet.Address_Octet_Type'Value
                  (Cloud_Multicast_Address_Env (13 .. 15))));
   begin

      Ada.Text_IO.Put_Line ("Hardware Initialize");

      -- initialize the interfaces
      Radio :=
        new Drivers.Ethernet.Ethernet_Type
          (Cloud_Address, 3_000, Cloud_Multicast_Address);
      Cloud :=
        new Drivers.Ethernet.Ethernet_Type
          (Cloud_Address, 4_000, Cloud_Multicast_Address);

      -- initialize the interfaces
      Radio.Initialize;
      Cloud.Initialize;

   end Initialize;

   procedure Schedule (Cycle : Types.Schedule.Cycle_Type) is
   begin

      case Cycle is

         when others =>
            null;

      end case;

   end Schedule;

end Hardware;
