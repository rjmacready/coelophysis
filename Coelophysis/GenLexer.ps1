# Lexer.fsl --> Lexer.fs

# if final file doesn't exists 
# or the original file modify date > final file modify date

param(  [Parameter(Mandatory=$True, Position=1)]
        [string] $project_dir)

# For some reason, a " is append to the end of $project_dir
# *sigh*
if($project_dir[$project_dir.Length - 1] -eq '"') {
    $project_dir = $project_dir.Substring(0, $project_dir.Length - 1)
}
"LEXER: >$project_dir<" | out-string
        
$final_file = "Lexer.fs"
$final_file = [System.IO.Path]::Combine($project_dir, $final_file)

$original_file = "Lexer.fsl"
$original_file = [System.IO.Path]::Combine($project_dir, $original_file)

if(Test-Path $final_file ) {
    "final file exists ..." | out-string
    
    $final = Get-Item $final_file
    $original = Get-Item $original_file
    
    if($original.LastWriteTime -lt $final.LastWriteTime) {
        "final is more recent than original, no need to generate ..." | out-string
        exit
    }
}

"generating..." | out-string
start-process """C:\Program Files (x86)\FSharpPowerPack-4.0.0.0\bin\fslex""" "--unicode ""$original_file""" -nonewwindow -wait
