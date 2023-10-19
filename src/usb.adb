with Interfaces.C;
use Interfaces.C;
with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with USB.Low;
with Ada.Text_IO;
use Ada.Text_IO;

package body USB is

   function Error_Text (Error_Code : Interfaces.C.int) return String is

      Text : constant Interfaces.C.Strings.chars_ptr :=
        USB.Low.Error_Text (Error_Code);

   begin

      return Interfaces.C.Strings.Value (Text);

   end Error_Text;

   procedure Check_Error (Error_Code : Interfaces.C.int) is

      function Error_From_Code is new Ada.Unchecked_Conversion
        (Interfaces.C.int, USB.Low.Error);
      Err  : constant USB.Low.Error := Error_From_Code (Error_Code);
      Text : constant String        := Error_Text (Error_Code);

   begin

      if not Err'Valid then
         return;
      end if;

      case Err is
         when USB.Low.Other_Error =>
            raise Other_Error with Text;
         when USB.Low.Operation_Not_Supported_Error =>
            raise Operation_Not_Supported_Error with Text;
         when USB.Low.Out_Of_Memory_Error =>
            raise Out_Of_Memory_Error with Text;
         when USB.Low.Syscall_Interrupted_Error =>
            raise Syscall_Interrupted_Error with Text;
         when USB.Low.Pipe_Error =>
            raise Pipe_Error with Text;
         when USB.Low.Overflow_Error =>
            raise Overflow_Error with Text;
         when USB.Low.Timeout_Error =>
            raise Timeout_Error with Text;
         when USB.Low.Resource_Busy_Error =>
            raise Resource_Busy_Error with Text;
         when USB.Low.Not_Found_Error =>
            raise Not_Found_Error with Text;
         when USB.Low.No_Device_Error =>
            raise No_Device_Error with Text;
         when USB.Low.Access_Denied_Error =>
            raise Access_Denied_Error with Text;
         when USB.Low.Invalid_Parameter_Error =>
            raise Invalid_Parameter_Error with Text;
         when USB.Low.Input_Output_Error =>
            raise Input_Output_Error with Text;
         when others =>
            null;
      end case;

   end Check_Error;

   function Make_Context return Context is

      Ctx  : Context;
      Data : Context_Contents;

   begin

      Check_Error (USB.Low.Init (Data.Address));
      Ctx.Set (Data);
      return Ctx;

   end Make_Context;

   procedure Context_Release (Self : in out Context_Contents) is
   begin

      USB.Low.Deinit (Self.Address);

   end Context_Release;

end USB;
