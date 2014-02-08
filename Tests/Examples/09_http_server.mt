
url "http://127.0.0.1:9876/" | 
	http_server _ | 
		server := _ | 
			start server |
				set server | 
					_ &on_context |
						_ fun ctx -> (
							resp := (get ctx | _ &Response)  |
							outstream := (get resp | _ &OutputStream ) | 
							write outstream |
							_ "teste" |
							close outstream
						) | sleep 60000 | stop server;

