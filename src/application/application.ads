with Library.Network;
with Library.Telemetry;

with Types.Schedule;

package Application is

   Network   : Library.Network.Network_Access_Type;
   Telemetry : Library.Telemetry.Telemetry_Access_Type;

   procedure Initialize;

   procedure Schedule (Cycle : Types.Schedule.Cycle_Type);

end Application;
