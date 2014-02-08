# Parser.fsi, Parser.fsy --> Parser.fs

# if final file doesn't exists 
# or the original file modify date > final file modify date

param(  [Parameter(Mandatory=$True, Position=1)]
        [string] $project_dir)

# For some reason, a " is append to the end of $project_dir
# *sigh*
if($project_dir[$project_dir.Length - 1] -eq '"') {
    $project_dir = $project_dir.Substring(0, $project_dir.Length - 1)
}
     
"PARSER: >$project_dir<" | out-string

$final_file = "Parser.fs"
$final_file = [System.IO.Path]::Combine($project_dir, $final_file)

$original_file = "Parser.fsy"
$original_file = [System.IO.Path]::Combine($project_dir, $original_file)

"original: $original_file, final: $final_file" | out-string

if(Test-Path $final_file ) {
    "final file exists ..." | out-string

    $original = Get-Item $original_file    
    $final = Get-Item $final_file
    
    if($original.LastWriteTime -lt $final.LastWriteTime) {
        "final is more recent than original, no need to generate ..." | out-string
        exit
    }
}

"generating >$original_file< ..." | out-string
start-process """C:\Program Files (x86)\FSharpPowerPack-4.0.0.0\bin\fsyacc""" "--module Parser ""$original_file""" -nonewwindow -wait
