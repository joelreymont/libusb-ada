pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C;
with Interfaces.C.Strings;
with System;

package USB.Low is

  type Error is
   (Other_Error,
    Operation_Not_Supported_Error,
    Out_Of_Memory_Error,
    Syscall_Interrupted_Error,
    Pipe_Error,
    Overflow_Error,
    Timeout_Error,
    Resource_Busy_Error,
    Not_Found_Error,
    No_Device_Error,
    Access_Denied_Error,
    Invalid_Parameter_Error,
    Input_Output_Error,
    Success);
  for Error use
   (Other_Error                   => -99,
    Operation_Not_Supported_Error => -12,
    Out_Of_Memory_Error           => -11,
    Syscall_Interrupted_Error     => -10,
    Pipe_Error                    => -9,
    Overflow_Error                => -8,
    Timeout_Error                 => -7,
    Resource_Busy_Error           => -6,
    Not_Found_Error               => -5,
    No_Device_Error               => -4,
    Access_Denied_Error           => -3,
    Invalid_Parameter_Error       => -2,
    Input_Output_Error            => -1,
    Success                       => 0);
  for Error'Size use Interfaces.C.int'Size;

  function Init (Context : out System.Address) return Interfaces.C.int with
   Import => True, Convention => C, External_Name => "libusb_init";

  procedure Deinit (Context : System.Address) with
   Import => True, Convention => C, External_Name => "libusb_exit";

  function Error_Text
   (Error_Code : Interfaces.C.int) return Interfaces.C.Strings.chars_ptr with
   Import => True, Convention => C, External_Name => "libusb_strerror";

end USB.Low;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
