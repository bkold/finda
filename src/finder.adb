with Ada.Strings.Unbounded;

package body Finder is

	procedure Find_Start (Directory : in String) is
		Thread_Check_Return : Thread_Access;
	begin
		loop
			Task_Pool.Check_Out(Thread_Check_Return);
			exit when Thread_Check_Return /= Null;
			delay 0.0;
		end loop;
		Thread_Check_Return.Run(Directory, 0);
	end;

	procedure Find (
		Directory : in String;
		Depth : in Natural)
	is
		Local_Search : Directory_Type;
		D_Entry : Directory_Entry;
	begin
		Local_Search := Open(Directory);
		if Local_Search /= Null_Directory then
			loop
				D_Entry := Read(Local_Search);
				exit when D_Entry = Null_Entry;

				declare
					Entry_Name : constant String := Name(D_Entry);
					Full_Entry_Name : constant String := Directory & '/' & Entry_Name;
					Thread_Check_Return : Thread_Access;
				begin
					if Match_All then
						if GNAT.Regexp.Match(Full_Entry_Name, Match_Token) then
							Write(Full_Entry_Name, Mode(D_Entry));
						end if;
					else
						if GNAT.Regexp.Match(Entry_Name, Match_Token) then
							Write(Full_Entry_Name, Mode(D_Entry));
						end if;
					end if;

					if Depth < Max_Depth then
						if Mode(D_Entry) = DIR and
						then Entry_Name /= ".." and then Entry_Name /= "." then
							Task_Pool.Check_Out(Thread_Check_Return);
							if Thread_Check_Return /= Null then
								Thread_Check_Return.Run(Full_Entry_Name, Depth+1);
							else
								Find(Full_Entry_Name, Depth+1);
							end if;
						end if;
					end if;
				end;
			end loop;

			Close(Local_Search);
		end if;
	end Find;


	task body Thread is
		use Ada.Strings.Unbounded;
		Directory : Unbounded_String;
		Local_Depth : Natural;
	begin
		loop
			select
				accept Run (D : in String; Depth : in Natural) do
					Directory := To_Unbounded_String(D);
					Local_Depth := Depth;
				end Run;
				Find(To_String(Directory), Local_Depth);
				Task_Pool.End_Thread(Num);
			or
				terminate;
			end select;
		end loop;
	end Thread;

	protected body Task_Pool is
		procedure Initialize is
		begin
			if not Is_Initialized then
				for I in CPU_Local'Range loop
					Threads(I) := new Thread(I);
					Thread_Stack.Push(Status, I);
				end loop;
				Is_Initialized := True;
			end if;
		end Initialize;

		procedure Check_Out (Thread_Pointer : out Thread_Access) is
			use Thread_Stack;
		begin
			if not Is_Empty(Status) then
				Thread_Pointer := Threads(Pop(Status));
			else
				Thread_Pointer := Null;
			end if;
		end Check_Out;

		procedure End_Thread (Thread_Num : in CPU_Local) is
		begin
			Thread_Stack.Push(Status, Thread_Num);
		end End_Thread;
	end Task_Pool;

begin

	Task_Pool.Initialize;

end Finder;