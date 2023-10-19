pragma Ada_2022;

with Ada.Text_IO;
use Ada.Text_IO;
with USB;

procedure Enumerate is
   Ctx : constant USB.Context := USB.Make_Context;
begin
   Put_Line ("Ctx: " & Ctx'Image);
end Enumerate;
