=============================================================
SHINY APP - AJUSTE MÚLTIPLO DE ALTURA PARA INVENTÁRIO FLORESTAL
Desenvolvido por Lara R. Gualberto
=============================================================

=== DESCRIÇÃO ===
Este aplicativo Shiny realiza ajustes múltiplos de altura para dados de 
inventário florestal, testando diferentes modelos matemáticos para encontrar 
a melhor estimativa de altura das árvores.

=== REQUISITOS ===
- R instalado (versão 4.0.0 ou superior)
- RStudio (recomendado)
- Pacotes necessários:
  * shiny
  * shinythemes
  * shinyWidgets
  * fontawesome
  * openxlsx
  * DT

Para instalar os pacotes necessários, execute no R:
install.packages(c("shiny", "shinythemes", "shinyWidgets", 
                  "fontawesome", "openxlsx", "DT"))

=== FORMATO DOS DADOS DE ENTRADA ===
1. Planilha de Dados (Excel):
   - Primeira coluna: Parcela (identificador)
   - Segunda coluna: DAP - Diâmetro à Altura do Peito (cm)
   - Terceira coluna: Altura total da árvore (m)
   - Quarta coluna: Altura a ser estimada (0 para árvores sem medição)

2. Arquivo de Modelos (TXT):
   - Um modelo por linha
   - Formato: ht~dap, ht~I(1/dap^2), etc.
   - Exemplo:
     ht~dap
     ht~I(1/dap^2)
     ht~I(dap^2)
     ht~dap+I(dap^2)

=== COMO USAR ===
1. Execute o aplicativo no R/RStudio
2. Carregue a planilha Excel com os dados do inventário
3. Carregue o arquivo TXT com os modelos a serem testados
4. Clique em "Processar Dados"
5. Visualize os resultados na aba "Resultados"
6. Use os botões de exportação para salvar os resultados

=== FUNCIONALIDADES ===
O aplicativo oferece:
- Interface moderna e intuitiva
- Upload de dados via Excel
- Modelos personalizáveis via arquivo TXT
- Visualização dos modelos carregados
- Tabela de resultados interativa
- Opções de exportação (CSV, Excel, Copy)

=== RESULTADOS ===
O aplicativo fornece:
- Ajuste automático de todos os modelos
- Seleção do melhor modelo por parcela
- Estimativa das alturas faltantes
- Tabela completa com todos os dados processados

=== SUPORTE ===
Em caso de dúvidas ou problemas:
1. Verifique o formato dos dados de entrada
2. Certifique-se de que todas as colunas necessárias estão presentes
3. Confirme se os modelos estão escritos corretamente no arquivo TXT

=== CITAÇÃO SUGERIDA ===
Gualberto, L. R. (2024). Shiny App para Ajuste Múltiplo de Altura 
em Inventário Florestal.

=== LICENÇA ===
Este aplicativo é disponibilizado para uso acadêmico e profissional.
Por favor, cite adequadamente ao utilizar.

=== CONTATO ===
Para sugestões ou reportar problemas, entre em contato através do GitHub.

=============================================================
Última atualização: Janeiro/2025
============================================================= 