$env:PATH = "C:\Program Files\R\R-4.4.1\bin\x64;C:\Program Files\MiKTeX\miktex\bin\x64;" + $env:PATH
$env:QUARTO_R = "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe"
$env:R_LIBS_USER = "$env:LOCALAPPDATA\R\win-library\4.4"
Set-Location "D:\Journal"

# Garante que arquivos temporarios do R/Quarto/Microdados fiquem no SSD D: (evitando lotar o disco C:)
New-Item -ItemType Directory -Path "D:\Journal\dados\tmp_microdados" -Force | Out-Null
$env:TMP = "D:\Journal\dados\tmp_microdados"
$env:TEMP = "D:\Journal\dados\tmp_microdados"
$env:TMPDIR = "D:\Journal\dados\tmp_microdados"

# 1. Atualiza o banco de dados unificado (painel_dados.rds) a partir de scripts/atualizar_dados.R
& "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe" "scripts/atualizar_dados.R"

# 2. Compila automaticamente todos os boletins, cartas e dossies da equipe (R + LaTeX -> PDF)
& "C:\Program Files\R\R-4.4.1\bin\x64\Rscript.exe" "scripts/compilar_boletins.R"

# 3. Renderiza a Central de Boletins (news.html) e o Painel Principal (index.html)
& "C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe" render "news.qmd"
& "C:\Program Files\RStudio\resources\app\bin\quarto\bin\quarto.exe" render "index.qmd"

# 4. Limpa arquivos temporarios de sessao no SSD D:
Remove-Item "D:\Journal\dados\tmp_microdados\*" -Recurse -Force -ErrorAction SilentlyContinue
