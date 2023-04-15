with Drivers.Ethernet;

with Types.Schedule;

package Hardware is

   Radio : Drivers.Ethernet.Ethernet_Access_Type;
   Cloud : Drivers.Ethernet.Ethernet_Access_Type;

   procedure Initialize;

   procedure Schedule (Cycle : Types.Schedule.Cycle_Type);

end Hardware;
