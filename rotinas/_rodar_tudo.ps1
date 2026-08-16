# Reexecuta o pipeline de revisao inteiro, na ordem de dependencia.
# Uso:  powershell -File rotinas\_rodar_tudo.ps1
# Os passos 0 e 1 (FAC) nao dependem da especificacao do modelo estrutural e
# nao sao reexecutados aqui; rode 17_fac_pseudo_erros.R manualmente se a base
# de pseudo-erros mudar.

$ErrorActionPreference = "Stop"
$R    = "C:\Program Files\R\R-4.3.2\bin\Rscript.exe"
$repo = Split-Path -Parent $PSScriptRoot
$art  = Join-Path (Split-Path -Parent (Split-Path -Parent $repo)) "Artigo Estratos Geográficos\Versão atual"
$env:REPO_RAIZ  = $repo
$env:ARTIGO_DIR = $art
$log = Join-Path $repo "outputs\log_pipeline.txt"
"=== inicio: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss') ===" | Set-Content $log -Encoding UTF8

function Passo($nome, $script, $extra = @{}) {
  foreach ($k in $extra.Keys) { Set-Item -Path "env:$k" -Value $extra[$k] }
  "--- $nome  [$(Get-Date -Format 'HH:mm:ss')]" | Add-Content $log -Encoding UTF8
  & $R (Join-Path $repo "rotinas\$script") 2>&1 | Add-Content $log -Encoding UTF8
  if ($LASTEXITCODE -ne 0) { "ABORTOU em $nome" | Add-Content $log -Encoding UTF8; exit 1 }
}

Passo "passo 2  identificacao"      "20_identificacao_paralela.R"
Passo "passo 2b especificacao"      "21_especificacao_final.R"
Passo "passo 3  univariados"        "22_univariado_final.R"
foreach ($ind in @("desocupados", "ocupados", "taxa")) {
  Passo "passo 4  multivariado $ind" "23_multivariado_final.R" @{ INDICADOR = $ind; QUENTE = "0"; MAXIT = "3000" }
}
Passo "passo 5  taxa"               "24_taxa_final.R"
Passo "passo 6  saidas do artigo"   "25_saidas_artigo.R"

"=== fim: $(Get-Date -Format 'yyyy-MM-dd HH:mm:ss') ===" | Add-Content $log -Encoding UTF8
