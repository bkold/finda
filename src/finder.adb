with Dirent; use Dirent;
with Ada.Text_IO;
with Ada.Strings.Fixed;

package body Finder is

	procedure Find_Start 
		(Directory, Token : in String; 
		 Desired_Mode : in Search_Mode;
		 Desired_Depth : in Natural := Natural'Last)
	is
	begin
		Run_Mode := Desired_Mode;
		Max_Depth := Desired_Depth;
		if Run_Mode = Regular_Expression then
			Search_Pattern := GNAT.Regexp.Compile(Token, True);
		end if;
		Find(Directory, Token);
	end;

	procedure Find (Directory, Token : in String) is
		Local_Search : Directory_Type;
		D_Entry : Directory_Entry;
		Error_Code : Integer;
	begin
		Local_Search := Open(Directory);
		if Local_Search /= Null_Directory then
			loop
				D_Entry := Read(Local_Search);
				exit when D_Entry = Null_Entry;

				if Run_Mode = Plain and 
				then Ada.Strings.Fixed.Index(Name(D_Entry), Token) > 0 then
					Write(Directory & "/" & Name(D_Entry));
				elsif Run_Mode = Regular_Expression and
				then GNAT.Regexp.Match(Name(D_Entry), Search_Pattern) then
					Write(Directory & "/" & Name(D_Entry));
				end if;

				if Depth < Max_Depth then
					if Mode(D_Entry) = DIR and
					then Name(D_Entry) /= ".." and then Name(D_Entry) /= "." then
						Depth := Depth + 1;
						Find(Directory & "/" & Name(D_Entry), Token);
						Depth := Depth - 1;
					end if;
				end if;
			end loop;

			Error_Code := Close(Local_Search);
			if Error_Code /= 0 then
				raise Dir_Error;
			end if;
		end if;

	end Find;

	procedure Write (Directory : in String) is
	begin
		Ada.Text_IO.Put_Line(Directory);
	end Write;

end Finder;