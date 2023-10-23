pragma Ada_2022;

with Ada.Text_IO; use Ada.Text_IO;
with USB;
with System;

procedure Enumerate is
begin

   USB.Init;

   declare
      Ctx     : constant USB.Context     := USB.Make_Context;
      Devices : constant USB.Device_List := USB.Get_Device_List (Ctx);
   begin

      Put_Line ("Devices: " & Devices'Image);
      Put_Line ("System.Storage_Unit = " & System.Storage_Unit'Image);

   end;

end Enumerate;
