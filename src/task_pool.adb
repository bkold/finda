package body Task_Pool is

	task body Thread is
	begin
		null;
		-- accept Run (Directory, Token : in String) do
		-- 	Worker(Directory, Token);
		-- end Run;
	end Thread;

	protected body Task_Pool_Status is
		function Has_Empty return CPU_Range is
		begin
			for I in Tasks_Status_Array'Range loop
				null;
				-- if Tasks_Status_Array(I) = False then
				-- 	Tasks_Status_Array(I) := True;
				-- 	return I;
				-- end if;
			end loop;

			return CPU_Range'First;
		end Has_Empty;

		procedure Free (Task_Number : in CPU) is
		begin
			Tasks_Status_Array(Task_Number) := False;
		end Free;
	end Task_Pool_Status;

end Task_Pool;