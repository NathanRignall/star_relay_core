with Library.Network;

with Types.Physics;
with Types.Schedule;
with Types.State;

package Library.Telemetry is

   type Variant_Type is (Debug, Location);
   for Variant_Type use (Debug => 0, Location => 1);
   for Variant_Type'Size use 1;
   Variant_Default : constant Variant_Type := Debug;

   type Debug_Type is record
      null;
   end record;

   type Location_Type is record
      Position : Types.Physics.Position_Type;
      Velocity_Vector : Types.Physics.Velocity_Vector_Type;
      Rotation_Vector : Types.Physics.Rotation_Vector_Type;
   end record;
   for Location_Type use record
      Position at 0 * 16 range 0 .. 95;
      Velocity_Vector at 0 * 16 range 96 .. 191;
      Rotation_Vector at 0 * 16 range 192 .. 287;
   end record;
   for Location_Type'Size use 288;

   type Telemetry_Packet_Type (Variant : Variant_Type) is record
      case Variant is
         when Debug =>
            Debug : Debug_Type;
         when Location =>
            Location : Location_Type;
      end case;
   end record;
   for Telemetry_Packet_Type use record
      Variant  at 0 * 16 range 0 ..  1;
      Debug    at 1 * 16 range 0 ..  15;
      Location at 1 * 16 range 0 .. 287;
   end record;
   for Telemetry_Packet_Type'Size use 417;

   type Telemetry_Type (Network : Library.Network.Network_Access_Type) is
     tagged private;

   type Telemetry_Access_Type is access all Telemetry_Type;

   procedure Initialize (This : Telemetry_Type);

   procedure Schedule
     (This : Telemetry_Type; Cycle : Types.Schedule.Cycle_Type);

private

   type Telemetry_Type (Network : Library.Network.Network_Access_Type) is
   tagged record
      null;
   end record;

end Library.Telemetry;
