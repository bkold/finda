package Dirent.Writing_Schemes is

	type Write_Procedure_Pointer is access procedure (Directory : in String; Mode : in Directory_Mode);

	procedure Write_Plain (Directory : in String; Mode : in Directory_Mode);

	procedure Write_Quotes (Directory : in String; Mode : in Directory_Mode);

	procedure Write_Quotes_One (Directory : in String; Mode : in Directory_Mode);

	procedure Write_Colors (Directory : in String; Mode : in Directory_Mode);

end Dirent.Writing_Schemes;