with Dirent; use Dirent;
with Ada.Text_IO;

package body Finder is

	procedure Find_Start (
		Directory, Token : in String; 
		Desired_Depth : in Natural := Natural'Last)
	is
	begin
		Max_Depth := Desired_Depth;
		Match_Token := GNAT.Regexp.Compile(Token, True);
		Find(Directory, 0);
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
				begin
					if GNAT.Regexp.Match(Entry_Name, Match_Token) then
						Write(Directory & '/' & Entry_Name);
					end if;

					if Depth < Max_Depth then
						if Mode(D_Entry) = DIR and
						then Entry_Name /= ".." and then Entry_Name /= "." then
							Find(Directory & '/' & Entry_Name, Depth+1);
						end if;
					end if;
				end;
			end loop;

			Close(Local_Search);
		end if;
	end Find;

	procedure Write (Directory : in String) is
	begin
		Ada.Text_IO.Put_Line(Directory);
	end Write;

end Finder;