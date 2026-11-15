# Jornal Econômico do Tocantins

Painel e portal de conjuntura econômica do Estado do Tocantins (**Autor:** Lorenzo Costa | Graduado em Economia), construído com **Quarto Dashboard**, **R (Plotly / DT)** e **JavaScript em Tempo Real**.

---

## 🏛️ Arquitetura Híbrida do Projeto

O jornal combina duas camadas de atualização de dados:

1. **Indicadores Estruturais (Rotina em R — `scripts/atualizar_dados.R`):**
   - Extrai e organiza dados macroeconômicos e regionais (PIB do Tocantins, Mercado de Trabalho / CAGED / RAIS / PNAD, Comércio Exterior / Comex Stat, Arrecadação ICMS / SEFAZ-TO, Dinâmica Empresarial / Receita Federal, Taxa Selic e IPCA via API oficial do Banco Central).
   - Salva os dados consolidados em `dados/painel_dados.rds` e `dados/indicadores_to.json`.
   - Controla automaticamente o selo vermelho **`NOVO`** (`#d85c5c`): sempre que uma base de dados muda, a data daquela seção é atualizada para o dia da coleta e o selo `NOVO` fica ativo por 3 dias no painel.

2. **Cotações em Tempo Real (Navegador / JavaScript — 24/7):**
   - **Faixa Ao Vivo no Topo (Live Ticker)** e aba dedicada **`Câmbio e Mercados`**.
   - Atualiza sozinha a cada **60 segundos** diretamente no navegador do leitor (sem precisar rodar o R novamente):
     - **Câmbio Comercial e Turismo:** Dólar (`USD/BRL`), Euro (`EUR/BRL`) e Bitcoin (`BTC/BRL`) + série histórica dos últimos 15 dias em gráfico Plotly interativo (via *AwesomeAPI*).
     - **Bolsa de Valores:** Ibovespa (`IBOV` via *Brapi*).
     - **Commodities Agrícolas (Referência TO / MATOPIBA):** Soja (Saca 60kg), Boi Gordo (Arroba 15kg) e Milho (Saca 60kg) convertidos em tempo real pelo câmbio comercial.

---

## 📂 Estrutura de Pastas

```text
Journal/
├── index.qmd                         # Código-fonte do Painel (Quarto Dashboard)
├── index.html                        # Site final gerado (pronto para GitHub Pages)
├── news.qmd                          # Código-fonte do Caderno de Notícias / Boletins
├── scripts/
│   └── atualizar_dados.R             # Rotina em R para coleta e atualização dos dados
├── dados/
│   ├── painel_dados.rds              # Base consolidada lida pelo index.qmd
│   └── indicadores_to.json           # Base exportada em JSON
└── .github/
    └── workflows/
        └── atualizar_jornal.yml      # Automação diária no GitHub Actions
```

---

## 🚀 Como Atualizar os Dados pelo R

### Opção 1: Pelo seu computador (RStudio / Positron)
1. Abra o arquivo `scripts/atualizar_dados.R` para editar séries ou conectar novas planilhas/APIs e execute o script.
2. Em seguida, clique em **Render** no `index.qmd` (ou rode `quarto render index.qmd`).

### Opção 2: Automático no GitHub (GitHub Actions)
O arquivo `.github/workflows/atualizar_jornal.yml` já está configurado para:
- Rodar o `scripts/atualizar_dados.R` e renderizar o `index.html` **automaticamente todos os dias às 08:00 (horário de Brasília)**.
- Ou rodar a qualquer momento clicando na aba **Actions** do seu repositório no GitHub $\rightarrow$ **Atualizar Dados R e Publicar Jornal** $\rightarrow$ **Run workflow**.

---

## 🌐 Como Hospedar no GitHub Pages e Conectar Domínio Pago

### 1. Ativar no GitHub Pages (Gratuito)
1. Suba os arquivos desta pasta (`Journal`) para um repositório no seu GitHub.
2. No GitHub, acesse **Settings** $\rightarrow$ **Pages**.
3. Em **Build and deployment** $\rightarrow$ **Source**, selecione **Deploy from a branch**.
4. Escolha a branch `main` (ou `master`) e a pasta `/ (root)` e clique em **Save**.
5. Seu jornal já estará no ar em `https://seu-usuario.github.io/nome-do-repositorio/`!

### 2. Migrar para Domínio Pago Próprio (ex: `jornaleconomicoto.com.br`)
Quando quiser usar um domínio próprio (comprado no **Registro.br**, Hostinger, Cloudflare, etc.):
1. No painel de DNS onde comprou o domínio, crie um registro `CNAME` apontando `www` para `seu-usuario.github.io`.
2. Na aba **Settings $\rightarrow$ Pages** do GitHub, digite o seu domínio no campo **Custom domain**, clique em **Save** e marque a opção **Enforce HTTPS**.
3. O GitHub criará automaticamente o arquivo `CNAME` na pasta e fornecerá o certificado de segurança SSL gratuito!
# journal_economics
