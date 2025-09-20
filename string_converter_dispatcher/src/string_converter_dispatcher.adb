-- Cristina Fernandez
-- CSC 435 Programming Languages
-- Ada String Converter with Task-Based Dispatcher

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Command_Line;
with Ada.Integer_Text_IO;

procedure String_Converter_Dispatcher is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   
   -- Task type for worker tasks that will handle individual conversions
   task type Worker_Task is
      -- Entry point to receive work assignment form dispatcher
      entry Start_Work(Choice : in Integer; Input_String : in Unbounded_String; Task_ID : in Integer);
      -- Entry point for graceful shutdown signal
      entry Shutdown;
   end Worker_Task;
   
   -- Pointer type for dynamic task allocation
   type Worker_Access is access Worker_Task;
   
   -- Function to convert a character to hexadecimal string
   function To_Hex(C : Character) return String is
      Value : Natural := Character'Pos(C);
      Hex_Digits : constant String := "0123456789ABCDEF";
      Result : String(1..2); -- fixed size result string
   begin
      Result(1) := Hex_Digits(Value / 16 + 1);
      Result(2) := Hex_Digits(Value mod 16 + 1);
      return Result;
   end To_Hex;
   
   -- Function to convert a character to binary string
   function To_Binary(C : Character) return String is
      Value : Natural := Character'Pos(C);
      Result : String(1..8) := (others => '0');
      Temp : Natural := Value;
   begin
      for I in reverse Result'Range loop
         if Temp mod 2 = 1 then
            Result(I) := '1';
         end if;
         Temp := Temp / 2; -- Shift right (integer division)
      end loop;
      return Result;
   end To_Binary;
   
   -- Procedure to convert and display string in ASCII decimal
   procedure Convert_To_ASCII_Decimal(Input_String : Unbounded_String; Task_ID : Integer) is
      Str : constant String := To_String(Input_String);
   begin
      Put_Line("Task" & Integer'Image(Task_ID) & " - ASCII Decimal conversion of '" & Str & "':");
      for I in Str'Range loop
         Put_Line("  '" & Str(I) & "' -> " & Natural'Image(Character'Pos(Str(I))));
      end loop;
      Put_Line("Task" & Integer'Image(Task_ID) & " - ASCII Decimal conversion completed.");
      New_Line;
   end Convert_To_ASCII_Decimal;
   
   -- Procedure to convert and display string in hexadecimal
   procedure Convert_To_Hexadecimal(Input_String : Unbounded_String; Task_ID : Integer) is
      Str : constant String := To_String(Input_String);
   begin
      Put_Line("Task" & Integer'Image(Task_ID) & " - Hexadecimal conversion of '" & Str & "':");
      for I in Str'Range loop
         Put_Line("  '" & Str(I) & "' -> 0x" & To_Hex(Str(I)));
      end loop;
      Put_Line("Task" & Integer'Image(Task_ID) & " - Hexadecimal conversion completed.");
      New_Line;
   end Convert_To_Hexadecimal;
   
   -- Procedure to convert and display string in binary
   procedure Convert_To_Binary_Display(Input_String : Unbounded_String; Task_ID : Integer) is
      Str : constant String := To_String(Input_String);
   begin
      Put_Line("Task" & Integer'Image(Task_ID) & " - Binary conversion of '" & Str & "':");
      for I in Str'Range loop
         Put_Line("  '" & Str(I) & "' -> " & To_Binary(Str(I)));
      end loop;
      Put_Line("Task" & Integer'Image(Task_ID) & " - Binary conversion completed.");
      New_Line;
   end Convert_To_Binary_Display;
   
   -- Worker task body implementation
   task body Worker_Task is
      My_Choice : Integer;
      My_String : Unbounded_String;
      My_ID : Integer;
      Active : Boolean := True;  -- Controls main task loop
   begin
      -- Main task loop - continues until shutdown signal received
      while Active loop
         select
            accept Start_Work(Choice : in Integer; Input_String : in Unbounded_String; Task_ID : in Integer) do
               My_Choice := Choice;
               My_String := Input_String;
               My_ID := Task_ID;
            end Start_Work;
            
            -- Perform the conversion based on choice
            case My_Choice is
               when 1 =>
                  Convert_To_ASCII_Decimal(My_String, My_ID);
               when 2 =>
                  Convert_To_Hexadecimal(My_String, My_ID);
               when 3 =>
                  Convert_To_Binary_Display(My_String, My_ID);
               when others =>
                  Put_Line("Task" & Integer'Image(My_ID) & " - Invalid choice:" & Integer'Image(My_Choice));
                  Put_Line("Task" & Integer'Image(My_ID) & " - Valid choices are 1 (ASCII), 2 (Hex), 3 (Binary)");
            end case;
            
         or
            accept Shutdown do
               Active := False;  -- This will exit the while loop
            end Shutdown;
         end select;
      end loop;
      
      -- Task cleanup and termination messag
      Put_Line("Task" & Integer'Image(My_ID) & " - Worker task shutting down.");
   end Worker_Task;
   
   -- Main dispatcher variables
   Input_File : File_Type;
   Line : Unbounded_String;
   Choice : Integer;
   Input_String : Unbounded_String;
   Task_Counter : Integer := 0;
   Comma_Position : Natural;
   Choice_String : Unbounded_String;
   
   -- Array to keep track of worker tasks (for shutdown)
   type Worker_Array is array(1..100) of Worker_Access;
   Workers : Worker_Array;
   Worker_Count : Integer := 0;
   
begin
   Put_Line("=== Ada String Converter with Dispatcher ===");
   Put_Line("Processing file with concurrent worker tasks...");
   New_Line;
   
   -- Check command line arguments (exactly one argument)
   if Ada.Command_Line.Argument_Count /= 1 then
      Put_Line("Error: Please provide an input file name.");
      Put_Line("Usage: " & Ada.Command_Line.Command_Name & " <input_file>");
      Put_Line("Example: ./string_converter input.txt");
      return;
   end if;
   
   -- Try to open the input file
   begin
      Open(Input_File, In_File, Ada.Command_Line.Argument(1));
      Put_Line("Dispatcher - Successfully opened file: " & Ada.Command_Line.Argument(1));
   exception
      when Name_Error =>
         Put_Line("Error: Could not open file '" & Ada.Command_Line.Argument(1) & "'");
         Put_Line("Please check that the file exists and try again.");
         return;
      when others =>
         Put_Line("Error: An unexpected error occurred while opening the file.");
         return;
   end;
   
   -- Process file line by line
   while not End_Of_File(Input_File) loop
      Line := To_Unbounded_String(Get_Line(Input_File));
      
      -- Skip empty lines
      if Length(Line) = 0 then
         goto Continue_Loop;
      end if;
      
      -- Find comma position
      Comma_Position := Index(Line, ",");
      
      if Comma_Position = 0 then
         Put_Line("Dispatcher - Warning: Invalid line format (missing comma): " & To_String(Line));
         goto Continue_Loop;
      end if;
      
      -- Extract choice and string
      Choice_String := Head(Line, Comma_Position - 1);
      Input_String := Tail(Line, Length(Line) - Comma_Position);
      
      -- Convert choice to integer
      begin
         Choice := Integer'Value(To_String(Choice_String));
      exception
         when Constraint_Error =>
            Put_Line("Dispatcher - Warning: Invalid choice format: " & To_String(Choice_String));
            goto Continue_Loop;
      end;
      
      -- Increment task counter and create new worker task
      Task_Counter := Task_Counter + 1;
      Worker_Count := Worker_Count + 1;
      
      Put_Line("Dispatcher - Creating Task" & Integer'Image(Task_Counter) & 
               " for choice" & Integer'Image(Choice) & 
               " with string: '" & To_String(Input_String) & "'");
      
      -- Dynamically allocate and start worker task
      Workers(Worker_Count) := new Worker_Task;
      Workers(Worker_Count).Start_Work(Choice, Input_String, Task_Counter);
      
      <<Continue_Loop>>
   end loop;
   
   -- Close the file
   Close(Input_File);
   Put_Line("Dispatcher - File processing complete. Created" & Integer'Image(Task_Counter) & " worker tasks.");
   New_Line;
   
   -- Wait a moment for tasks to complete their work
   delay 2.0;
   
   -- Shutdown all worker tasks
   Put_Line("Dispatcher - Shutting down all worker tasks...");
   for I in 1..Worker_Count loop
      if Workers(I) /= null then
         Workers(I).Shutdown;
      end if;
   end loop;
   
   -- Give tasks time to shutdown gracefully
   delay 1.0;
   
   Put_Line("Dispatcher - All tasks have been notified to shut down.");
   Put_Line("Dispatcher - Program terminating.");
   
exception
   -- Gloval exception handler for unexpected errors
   when others =>
      Put_Line("Dispatcher - An unexpected error occurred.");
      if Is_Open(Input_File) then
         Close(Input_File);
      end if;
      
      -- Try to shutdown any active workers
      for I in 1..Worker_Count loop
         if Workers(I) /= null then
            Workers(I).Shutdown;
         end if;
      end loop;
      
end String_Converter_Dispatcher;