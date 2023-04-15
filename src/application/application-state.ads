with Types.Physics;
with Types.State;
package Application.State is

   use type Types.Physics.Velocity_Type;

   Core_State : Types.State.Core_State_Type :=
     Types.State.Core_State_Type'
       (Physical_State =>
          Types.State.Physical_State_Type'
            (Position                    =>
               Types.Physics.Position_Type'
                 (Latitude => 45.0, Longitude => 40.0, Altitude => 1_000.0),
             Acceleration_Vector         =>
               Types.Physics.Acceleration_Vector_Type'(0.5, 0.3, 0.1),
             Velocity_Vector             =>
               Types.Physics.Velocity_Vector_Type'(300.0, -100.0, 30.0),
             Rotation_Vector             =>
               Types.Physics.Rotation_Vector_Type'(0.0, 0.0, 0.0),
             Angular_Velocity_Vector     =>
               Types.Physics.Angular_Velocity_Vector_Type'(0.0, 0.0, 0.0),
             Angular_Acceleration_Vector =>
               Types.Physics.Angular_Acceleration_Vector_Type'
                 (0.0, 0.0, 0.0)));

   Aircraft_State : Types.State.Aircraft_Array_Type := Types.State.Aircraft_Array_Default;

end Application.State;
