-module(skyraid_fileop).

-export([upload_start/1, upload_content/3, download_start/1, download_content/1]).

upload_start(_FileName) ->
	skyraid_fileop_upload:start_link().

upload_content(FileRef, Content, ChunkID) ->
	skyraid_fileop_upload:content(FileRef, Content, ChunkID).

download_start(_FileName) ->
	skyraid_fileop_download:start_link().

download_content(FileRef) ->
	skyraid_fileop_download:download(FileRef).