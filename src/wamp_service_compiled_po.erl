%%EPO COMPILED FILE
%% -*- coding: utf-8 -*-
-module('wamp_service_compiled_po').
-compile(nowarn_unused_vars).
-compile(nowarn_unused_function).
-compile(nowarn_unused_record).

-record(porec2, {msgstr, msgstr_n = {}, n_max}).
-export([get_record/2, get_idx/2]).
-ignore_xref([get_record/2, get_idx/2]).
get_idx(N, <<"es_AR">>) ->
	to_integer(to_integer(N)
			      =/= to_integer(1));
get_idx(N, <<"en">>) ->
	to_integer(to_integer(N)
			      =/= to_integer(1));
get_idx(N, <<Locale2:2/binary, $_, _/binary>>) ->
	get_idx(N, Locale2);
get_idx(_, _) ->
	0.

get_record(Key, Locale) ->
	case Key of		<<"Unauthorized user."/utf8>> ->
			case Locale of
				<<"es_AR">> -> #porec2{msgstr = <<"Usuario no autorizado."/utf8>>, msgstr_n = {}, n_max = 0};
				<<"en">> -> #porec2{msgstr = <<"Unauthorized user."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"The user does not have the required permissions to access the resource."/utf8>> ->
			case Locale of
				<<"es_AR">> -> #porec2{msgstr = <<"El usuario no tiene los permisos necesarios para acceder al recurso."/utf8>>, msgstr_n = {}, n_max = 0};
				<<"en">> -> #porec2{msgstr = <<"The user does not have the required permissions to access the resource."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"Resource not found."/utf8>> ->
			case Locale of
				<<"es_AR">> -> #porec2{msgstr = <<"No se encontró el recurso."/utf8>>, msgstr_n = {}, n_max = 0};
				<<"en">> -> #porec2{msgstr = <<"Resource not found."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"There was an unknown error, please contact the administrator."/utf8>> ->
			case Locale of
				<<"es_AR">> -> #porec2{msgstr = <<"Hubo un error desconocido, póngase en contacto con el administrador."/utf8>>, msgstr_n = {}, n_max = 0};
				<<"en">> -> #porec2{msgstr = <<"There was an unknown error, please contact the administrator."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"The resource you are trying to retrieve does not exist."/utf8>> ->
			case Locale of
				<<"es_AR">> -> #porec2{msgstr = <<"No existe el recurso que está intentando recuperar."/utf8>>, msgstr_n = {}, n_max = 0};
				<<"en">> -> #porec2{msgstr = <<"The resource you are trying to retrieve does not exist."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"Unknown error."/utf8>> ->
			case Locale of
				<<"es_AR">> -> #porec2{msgstr = <<"Error desconocido."/utf8>>, msgstr_n = {}, n_max = 0};
				<<"en">> -> #porec2{msgstr = <<"Unknown error."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"Service timeout."/utf8>> ->
			case Locale of
				<<"es_AR">> -> #porec2{msgstr = <<"El servicio tardó demasiado."/utf8>>, msgstr_n = {}, n_max = 0};
				<<"en">> -> #porec2{msgstr = <<"Service timeout."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		<<"There was a timeout resolving the operation."/utf8>> ->
			case Locale of
				<<"es_AR">> -> #porec2{msgstr = <<"Se excedió el tiempo de espera de la operación."/utf8>>, msgstr_n = {}, n_max = 0};
				<<"en">> -> #porec2{msgstr = <<"There was a timeout resolving the operation."/utf8>>, msgstr_n = {}, n_max = 0};
			_ -> undefined
			end;
		_ -> undefined
	end.

to_integer(true) -> to_integer(1);
to_integer(false) -> to_integer(0);
to_integer(N) when is_integer(N) -> N.

to_boolean(true) -> true;
to_boolean(false) -> false;
to_boolean(N) when N > 0 -> to_boolean(true);
to_boolean(N) when N == 0 -> to_boolean(false).
	