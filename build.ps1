param([switch]$SkipAnalysis)

$ErrorActionPreference = 'Stop'
$taskRoot = $PSScriptRoot
$savedLocale = @{}
foreach ($key in @('LC_ALL', 'LC_CTYPE', 'LANG')) {
    $savedLocale[$key] = [Environment]::GetEnvironmentVariable($key, 'Process')
    [Environment]::SetEnvironmentVariable($key, 'C', 'Process')
}
Push-Location -LiteralPath $taskRoot
try {
    if (-not $SkipAnalysis) {
        & Rscript --vanilla Panagiotis_Housos_hw2.R
        if ($LASTEXITCODE -ne 0) { throw 'Homework analysis failed.' }
    }
    & Rscript --vanilla reproducibility/verify_hw2.R
    if ($LASTEXITCODE -ne 0) { throw 'Independent verification failed.' }
    & Rscript --vanilla report/export_results.R
    if ($LASTEXITCODE -ne 0) { throw 'Report export failed.' }
    & Rscript --vanilla reproducibility/record_environment.R
    if ($LASTEXITCODE -ne 0) { throw 'Environment recording failed.' }
    Push-Location -LiteralPath (Join-Path $taskRoot 'report')
    try {
        for ($pass = 1; $pass -le 2; $pass++) {
            & pdflatex -interaction=nonstopmode -halt-on-error Panagiotis_Housos_HW2_Report.tex
            if ($LASTEXITCODE -ne 0) { throw "LaTeX compilation failed on pass $pass." }
        }
    } finally { Pop-Location }
    Write-Output 'Built report/Panagiotis_Housos_HW2_Report.pdf'
} finally {
    Pop-Location
    foreach ($key in $savedLocale.Keys) {
        [Environment]::SetEnvironmentVariable($key, $savedLocale[$key], 'Process')
    }
}
