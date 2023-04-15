with Ada.Numerics.Elementary_Functions;

package body Types.Physics is

   function "+"
     (Left : Position_Type; Right : Displacement_Vector_Type)
      return Position_Type
   is
   begin
      return
        Position_Type'
          (Longitude =>
             Left.Longitude +
             Longitude_Type
               ((Scientific (Right (Y)) / Earth_Radius) * (180.0 / Pi)),
           Latitude  =>
             Left.Latitude +
             Latitude_Type
               ((Scientific (Right (X)) / Earth_Radius) * (180.0 / Pi) /
                Scientific
                  (Ada.Numerics.Elementary_Functions.Cos
                     (Float (Scientific (Left.Latitude) * Pi / 180.0)))),
           Altitude  => Left.Altitude + Altitude_Type (Right (Z)));
   end "+";

   function "+"
     (Left : Displacement_Vector_Type; Right : Position_Type)
      return Position_Type
   is
   begin
      return
        Position_Type'
          (Longitude =>
             Right.Longitude +
             Longitude_Type
               ((Scientific (Left (Y)) / Earth_Radius) * (180.0 / Pi)),
           Latitude  =>
             Right.Latitude +
             Latitude_Type
               ((Scientific (Left (X)) / Earth_Radius) * (180.0 / Pi) /
                Scientific
                  (Ada.Numerics.Elementary_Functions.Cos
                     (Float (Scientific (Right.Latitude) * Pi / 180.0)))),
           Altitude  => Right.Altitude + Altitude_Type (Left (Z)));
   end "+";

   function "+"
     (Left, Right : Velocity_Vector_Type) return Velocity_Vector_Type
   is
   begin
      return
        Velocity_Vector_Type'
          (Left (X) + Right (X), Left (Y) + Right (Y), Left (Z) + Right (Z));
   end "+";

   function "*"
     (Left : Velocity_Vector_Type; Right : Duration)
      return Displacement_Vector_Type
   is
   begin
      return
        Displacement_Vector_Type'
          (Displacement_Type (Left (X)) * Displacement_Type (Right),
           Displacement_Type (Left (Y)) * Displacement_Type (Right),
           Displacement_Type (Left (Z)) * Displacement_Type (Right));
   end "*";

   function "*"
     (Left : Duration; Right : Velocity_Vector_Type)
      return Displacement_Vector_Type
   is
   begin
      return
        Displacement_Vector_Type'
          (Displacement_Type (Left) * Displacement_Type (Right (X)),
           Displacement_Type (Left) * Displacement_Type (Right (Y)),
           Displacement_Type (Left) * Displacement_Type (Right (Z)));
   end "*";

   function "*"
     (Left : Acceleration_Vector_Type; Right : Duration)
      return Velocity_Vector_Type
   is
   begin
      return
        Velocity_Vector_Type'
          (Velocity_Type (Left (X)) * Velocity_Type (Right),
           Velocity_Type (Left (Y)) * Velocity_Type (Right),
           Velocity_Type (Left (Z)) * Velocity_Type (Right));
   end "*";

   function "*"
     (Left : Duration; Right : Acceleration_Vector_Type)
      return Velocity_Vector_Type
   is
   begin
      return
        Velocity_Vector_Type'
          (Velocity_Type (Left) * Velocity_Type (Right (X)),
           Velocity_Type (Left) * Velocity_Type (Right (Y)),
           Velocity_Type (Left) * Velocity_Type (Right (Z)));
   end "*";

end Types.Physics;
