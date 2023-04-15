with Ada.Environment_Variables;
with Ada.Text_IO;

package body Hardware is

   procedure Initialize is

      -- create the addresses for the interfaces
      Radio_Address : constant Drivers.Ethernet.Address_V4_Access_Type :=
        new Drivers.Ethernet.Address_V4_Type'(0, 0, 0, 0);

      Cloud_Address : constant Drivers.Ethernet.Address_V4_Access_Type :=
        new Drivers.Ethernet.Address_V4_Type'(0, 0, 0, 0);

   begin

      Ada.Text_IO.Put_Line ("Hardware Initialize");

      -- create the interfaces
      Radio := new Drivers.Ethernet.Ethernet_Type (Radio_Address, 8_000);
      Cloud := new Drivers.Ethernet.Ethernet_Type (Cloud_Address, 4_000);

      -- initialize the interfaces
      Radio.Initialize;
      Cloud.Initialize;

      --  -- join the multicast group for the radio
      --  Radio.Join_Multicast_Group (Radio_Multicast_Address.all);

   end Initialize;

   procedure Schedule (Cycle : Types.Schedule.Cycle_Type) is
   begin

      case Cycle is

         when others =>
            null;

      end case;

   end Schedule;

end Hardware;
