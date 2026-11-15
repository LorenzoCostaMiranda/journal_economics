# Guia Rápido da Equipe: Como Publicar um Novo Boletim (Sem precisar de R ou LaTeX)

Qualquer integrante da equipe pode publicar um **Boletim Flash**, um **Dossiê Anual** ou uma **Carta de Conjuntura** diretamente pelo navegador no GitHub, usando apenas um arquivo de texto simples (`.txt` ou `.md`).

O sistema do **Jornal Econômico do Tocantins** fará todo o trabalho pesado sozinho:
1. **Lê os dados oficiais** do Tocantins e do Brasil automaticamente (SEPLAN-TO, IBGE/SIDRA, BCB, Comex Stat, Novo CAGED, SEFAZ-TO);
2. **Escreve o parágrafo de Estatística Descritiva** trocando automaticamente as palavras (*crescimento/retração*, *alta/queda*, *superávit/déficit*, *liderado por...*);
3. **Desenha os gráficos vetoriais e monta as tabelas** no padrão do jornal;
4. **Junta com a sua análise humana**, compila o **PDF profissional em LaTeX** e **atualiza automaticamente o site** (*Giro de Boletins* e *Central de Publicações* para Boletins Flash e Dossiês Anuais).

---

## Passo a Passo em 3 Minutos (Pelo Próprio Site do GitHub)

1. Entre nesta pasta (`boletins/textos_equipe/`) no GitHub.
2. Clique no botão **"Add file"** (Adicionar arquivo) $\rightarrow$ **"Create new file"** (Criar novo arquivo).
3. Dê um nome para o arquivo seguindo o padrão `ANO_MES_tipo_tema.txt` (sem espaços ou acentos no nome do arquivo), por exemplo:
   - `2026_11_flash_comex.txt`
   - `2026_11_flash_trabalho.txt`
   - `2026_12_dossie_comex.txt`
   - `2026_11_carta_conjuntura.txt`
4. Copie a estrutura abaixo (ou do arquivo `_MODELO_PARA_EQUIPE.txt`), preencha os campos do topo e escreva sua análise normalmente em português:

```text
TIPO: flash_comex
TITULO: Exportações do Tocantins mantêm ritmo forte no mês
SUBTITULO: Embarques de grãos e proteína animal sustentam superávit comercial
AUTOR: Nome do Integrante | Equipe Jornal Econômico do Tocantins
DATA: 03/11/2026
PERIODO: Outubro/2026
DESTAQUE_CURTO: O saldo comercial acumulado atinge US$ 2,06 bilhões, consolidando a pauta exportadora estadual.
=== ANALISE_CONJUNTURA ===
Escreva aqui o seu primeiro parágrafo de análise humana, interpretando os números, o contexto de mercado, fatores climáticos, logísticos ou políticos.

Escreva quantos parágrafos desejar (basta deixar uma linha em branco entre um parágrafo e outro). Não precisa colocar nenhum código!

=== PERSPECTIVAS ===
Escreva aqui a visão da equipe para os próximos meses, tendências esperadas e pontos de atenção para o empresário e gestor público.
```

5. Clique no botão verde **"Commit changes..."** no canto superior direito. **Pronto!**

---

## Estrutura Padronizada de Temas e Linhas Editoriais (`TIPO:`)

O **Jornal Econômico do Tocantins** adota **8 temas padronizados** (espelhando as divisões do painel) para os **Boletins Flash** (mensais/trimestrais) e para os **Dossiês Anuais** (compilado anual dos resultados mensais), além da categoria **"Coringa"** para as **Cartas de Conjuntura**:

1. **Boletins Flash (Mensais / Trimestrais):** Formato **1 Coluna Executiva** | Paleta **Azul Marinho (`#0F2537`) & Dourado (`#C8942B`)** | **Acesso Público** (HTML completo no site + Download PDF).
2. **Dossiês Anuais (Compilado Anual):** Formato **Periódico / Revista em 2 Colunas** | Paleta **Bordô Imperial (`#4A1525`) & Dourado (`#C8942B`)** | **Acesso Público** (HTML completo no site + Download PDF).
3. **Cartas de Conjuntura ("Coringa" — Assinantes):** Formato **Periódico / Revista em 2 Colunas** | Paleta **Verde-Petróleo (`#12352B`) & Dourado (`#C8942B`)** | **Exclusivo para Assinantes por E-mail** (não entra no Giro do site; publicada a qualquer momento conforme saem pesquisas conjunturais ou estruturais mais extensas do IBGE, como PAM, PPM, PEVS, Contas Regionais, etc.).

| Tema Padronizado do Jornal | Código p/ Boletim Flash (Mensal/Trimestral) | Código p/ Dossiê Anual (Consolidado do Ano) |
| :--- | :--- | :--- |
| **1. Panorama Econômico do Tocantins** *(Overview Geral)* | `flash_panorama` | `dossie_panorama` |
| **2. PIB (TO)** | `flash_pib` | `dossie_pib` |
| **3. Mercado de Trabalho** | `flash_trabalho` *(ou `flash_caged`)* | `dossie_trabalho` *(ou `dossie_emprego`)* |
| **4. Comércio Exterior** | `flash_comex` | `dossie_comex` *(ou `fechamento_comex`)* |
| **5. Arrecadação e Finanças** | `flash_financas` *(ou `flash_icms`)* | `dossie_financas` |
| **6. Dinâmica Empresarial** | `flash_empresas` | `dossie_empresas` |
| **7. Câmbio e Mercados** | `flash_mercados` | `dossie_mercados` |
| **8. IPER-TO** | `flash_iper` | `dossie_iper` |
| **Carta de Conjuntura ("Coringa" — Pesquisas IBGE/Especiais)** | — | **`carta_conjuntura`** *(Exclusivo Assinantes)* |
