#DAS_week3
#数据分析的重要部分是传达分析结果。这通常采用表格或报告的形式，其中总结了数据分析过程和相关发现。在第3周的技能： Moodle的统计报告撰写部分中，有对统计分析之后编写统计报告的有用指南，希望您熟悉此资源，尤其是有关“结构”和“演示文稿”的部分。

#本周的交互式教程向您介绍R Markdown，R Markdown是一种非常有效的生成统计报告的方法，您将使用该方法在两个班级测试和“数据分析技能”中的小组项目中生成报告。夏天，您还将需要使用R Markdown生成有关您的Masters Project的报告。

install.packages('tinytex')
tinytex::install_tinytex()
# to uninstall TinyTeX, run tinytex::uninstall_tinytex() 

#注意：本节的编号与我们number_sections: yes在文档序言中设置的编号相同。如果您不希望为部分编号，请number_sections: no在序言中进行设置。我建议使用带编号的部分，因为这样可以更轻松地在文本中引用它们。

#可以为每个部分分配标签，以便可以在文本中引用它们。例如，要为“简介”部分添加标签，我们只需将标签添加{#sec:intro}到部分标题中，如下所示：
  
# Introduction {#sec:intro}

#在sec:intro此特定部分选择的名称在哪里。适当地标记您的节是一个好主意，以便以后引用它们时很容易。现在，可以使用\ref命令在文档的文本中引用此部分。那是
#Section \ref{sec:intro} ...

#会产生
#Section 1 ...
#其中1是可单击的超链接，它将带您到文档中该部分的开头。

install.packages("mvtnorm")

library(ggplot2)
library(dplyr)
library(moderndive)
library(gapminder)
library(skimr)
library(mvtnorm)
library(gridExtra)
library(kableExtra)
library(tidyr)

?evals









