# Reexecuta o pipeline de revisao na ordem de dependencia.
#
#   powershell -File rotinas\_rodar_tudo.ps1            # tudo, a partir do passo 2
#   powershell -File rotinas\_rodar_tudo.ps1 -De 3      # retoma do passo 3
#
# Os passos 0 e 1 (FAC dos pseudo-erros) nao dependem da especificacao do
# modelo estrutural e nao entram aqui; rode 17_fac_pseudo_erros.R manualmente
# se a base de pseudo-erros mudar.
#
# ATENCAO: NAO usar "& $R ... 2>&1 | Add-Content" com ErrorActionPreference =
# Stop. O Rscript escreve avisos benignos em stderr (p.ex. "package 'dlm' was
# built under R version ..."), o PowerShell os empacota em NativeCommandError e
# o script morre silenciosamente no meio da cadeia. Usa-se Start-Process com
# redirecionamento em arquivo, que nao sofre desse problema.

param([int]$De = 2)

$R    = "C:\Program Files\R\R-4.3.2\bin\Rscript.exe"
$repo = Split-Path -Parent $PSScriptRoot
# O manuscrito e versionado dentro do repositorio, em artigo/. Caminho sem
# acentos de proposito: o PowerShell 5.1 le arquivos .ps1 sem BOM como ANSI, e
# literais acentuados chegam corrompidos.
$art = Join-Path $repo "artigo"
if (-not (Test-Path -LiteralPath $art)) { throw "pasta do artigo nao encontrada: $art" }
$env:REPO_RAIZ  = $repo
$env:ARTIGO_DIR = $art

$log = Join-Path $repo "outputs\log_pipeline.txt"
$tmp = Join-Path $repo "outputs\_passo_atual.txt"
$err = Join-Path $repo "outputs\_passo_atual_err.txt"
"=== inicio: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss') (a partir do passo $De) ===" |
  Set-Content $log -Encoding UTF8

function Passo($ordem, $nome, $script, $extra = @{}) {
  if ($ordem -lt $De) { "--- pulado (passo $ordem): $nome" | Add-Content $log -Encoding UTF8; return }
  foreach ($k in $extra.Keys) { Set-Item -Path "env:$k" -Value $extra[$k] }
  "--- $nome  [$(Get-Date -Format 'HH:mm:ss')]" | Add-Content $log -Encoding UTF8
  $p = Start-Process -FilePath $R -ArgumentList "`"$(Join-Path $repo "rotinas\$script")`"" `
                     -RedirectStandardOutput $tmp -RedirectStandardError $err `
                     -NoNewWindow -Wait -PassThru
  Get-Content $tmp -Encoding UTF8 -ErrorAction SilentlyContinue | Add-Content $log -Encoding UTF8
  if ($p.ExitCode -ne 0) {
    "ABORTOU em '$nome' (exit $($p.ExitCode))" | Add-Content $log -Encoding UTF8
    Get-Content $err -Encoding UTF8 -ErrorAction SilentlyContinue | Add-Content $log -Encoding UTF8
    exit 1
  }
}

Passo 2 "passo 2  identificacao"      "20_identificacao_paralela.R"
Passo 3 "passo 2b especificacao"      "21_especificacao_final.R"
Passo 3 "passo 3  univariados"        "22_univariado_final.R"
foreach ($ind in @("desocupados", "ocupados", "taxa")) {
  Passo 4 "passo 4  multivariado $ind" "23_multivariado_final.R" @{ INDICADOR = $ind; QUENTE = "0"; MAXIT = "3000" }
}
Passo 5 "passo 5  taxa"               "24_taxa_final.R"
Passo 6 "passo 6  saidas do artigo"   "25_saidas_artigo.R"

Remove-Item $tmp, $err -ErrorAction SilentlyContinue
"=== fim: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss') ===" | Add-Content $log -Encoding UTF8
