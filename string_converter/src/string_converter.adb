with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;
with Ada.Characters.Latin_1;

procedure String_Converter is
use Ada.Text_IO;
use Ada.Strings.Unbounded;

-- Function converts character to hexadecimal string
function To_Hex(C : Character) return String is
-- Variable declarations come BEFORE begin
   Value : Natural := Character'Pos(C);
   Hex_Digits : constant String := "0123456789ABCDEF";
   Result : String(1..2); -- String of exactly 2 characters, indexed 1-2
begin
   -- Array indexing starts at 1 in Ada
   Result(1) := Hex_Digits(Value / 16 + 1);
   Result(2) := Hex_Digits(Value mod 16 + 1);
   return Result;
end To_Hex;

-- Function converts character to 8-bit binary string
function To_Binary(C : Character) return String is
   Value : Natural := Character'Pos(C);

   -- Initialized String with all '0' character
   Result : String(1..8) := (others => '0');
   Temp : Natural := Value;
begin
   -- "reverse" makes loop go backwards (8 down to 1)
   for I in reverse Result'Range loop
      if Temp mod 2 = 1 then
         Result(I) := '1';
      end if;
      Temp := Temp / 2; 
   end loop;
   return Result;
end To_Binary;

-- Procedure declarations (don't return values)

-- Precedure to display menu options
procedure Display_Menu is
begin
   New_Line;   --blank line
   Put_Line("=== Conversion Options ===");
   Put_Line("1. ASCII Decimal");
   Put_Line("2. Hexadecimal");
   Put_Line("3. Binary");
   Put_Line("4. Quit");
   Put("Enter your choice (1-4): ");   -- Put without new line
end Display_Menu;

procedure Convert_To_ASCII_Decimal(Input_String : Unbounded_String) is
   -- Converting Unbounded_String to reular String
   Str : constant String := To_String(Input_String);
begin
   Put_Line("ASCII Decimal conversion:");

   -- Loop through each character in string
   for I in Str'Range loop    --Str'Range gives valid index range of string
      -- Character'Pos converts character to its ASCII number
      Put_Line("'" & Str(I) & "' -> " & Natural'Image(Character'Pos(Str(I))));   -- Natural'Image converts number to string
   end loop;
end Convert_To_ASCII_Decimal;

-- Similar procedure for hexadecimal conversion
procedure Convert_To_Hexadecimal(Input_String : Unbounded_String) is
   Str : constant String := To_String(Input_String);
begin
   Put_Line("Hexadecimal conversion:");
   for I in Str'Range loop
      -- Calls To_Hex function for each character
      Put_Line("'" & Str(I) & "' -> 0x" & To_Hex(Str(I)));
   end loop;
end Convert_To_Hexadecimal;

-- Procedure for binary conversion
procedure Convert_To_Binary_Display(Input_String : Unbounded_String) is
   Str : constant String := To_String(Input_String);
begin
   Put_Line("Binary conversion:");
   for I in Str'Range loop
      -- Calls To_Binary function for each character
      Put_Line("'" & Str(I) & "' -> " & To_Binary(Str(I)));
   end loop;
end Convert_To_Binary_Display;

-- Main Program Variable Declarations
User_Input : Unbounded_String; -- dynamic string for user input 
Choice : Integer;    -- what user chooses from menu
Continue : Boolean := True;   -- loop control Variable

begin
   -- Program greeting
   Put_Line("=== Ada String Converter ===");
   Put_Line("This program converts strings to different number formats.");
   New_Line;

-- Main program loop - continues while Continue is True
while Continue loop
   -- Get string input from user
   Put("Enter a string to convert: ");

   -- Can declare variables in local blocks with declare blocks
   declare
      -- Get a line of input and store it
      Input_Line : constant String := Get_Line;
   begin
      User_Input := To_Unbounded_String(Input_Line);
   end;  -- End of declare block

   -- Check if user entered an empty string
   if Length(User_Input) = 0 then
      Put_Line("Warning: Empty string entered.");
   end if;

   -- Show menu and get user option
   Display_Menu;

   -- Exception Handling Block (built in)
   begin
      -- Read integer from user input 
      Ada.Integer_Text_IO.Get(Choice);
      Skip_Line;  -- Clear the input buffer

      -- Case Statement (switch)
      case Choice is
         when 1 =>
            Convert_To_ASCII_Decimal(User_Input);

         when 2 =>
            Convert_To_Hexadecimal(User_Input);

         when 3 =>
            Convert_To_Binary_Display(User_Input);

         when 4 => 
            Put_Line("You have now used String Converter!");
            Continue := False; --will exit while loop

         when others =>    -- Handles other integer values
            Put_Line("Invalid choice. Please enter 1,2,3, or 4.");
         end case;

      -- Exception Handler
      exception
         when others =>
            Put_Line("Invalid input. Please enter a number between 1 and 4.");
            Skip_Line;
      end;

      -- Pause between iterations if continuing
      if Continue then
         New_Line;
         Put_Line("Press Enter to continue...");
         Skip_Line;  -- Wait for user to press enter
      end if;
   end loop;

end String_Converter;   -- End of main procedure