# Loading libraries
library(foreign)
library(plyr)
library(reshape)
library(ggplot2)
library(stringr)
library(brew)
library(scales)
library(pander)
library(abind)

setwd('/home/sas/1irr/localgov2012/')
options(stringsAsFactors = F)
# Стоп на предупреждение
options(warn=2)

# Количество чисел после запятой в отчёте
round.digits <- 0
# Вывод системных сообщений в консоль, если не ноль, то выводить
verbose <- 1

# Load functions
source('~/1irr/functions/functions.R')
source('functions.r')

testdistr <- c("Псков", "Пушкиногорский район", "Бежаницкий район")
# Load raw data
source('load.r')
# datum <- datum[datum$district %in% testdistr, ]
# datum11 <- datum11[datum11$district %in% testdistr, ]

# Clean data
# source('clean.r')
# Привести к одному виду районы с менеджером и без

cat('\nНачинаем считать абсолютные частоты\n')
datacounts <- ddply(vargroups, .(vargroup), function(x) {aggrvars(x$varname, x$varlabel, datum, datum11, x$vargroup)})
cat('\nЗакончили считать абсолютные частоты\n')


verb('Удаляем данные по несуществующим главам администраций района и ненужным председателям собрания')
datacounts <- ddply(datacounts, .(district), function(x) {

										ifelse(district_heads$is_adm_hd[district_heads$district == x$district[[1]]],  {
														# Удалить по выбранному району всех глав районов2 и пред.рай.собр. dstr_hd2, dstr_assmb_hd
														x <- x[!(x$variable %in% getvarlab('dstr_hd2','dstr_assmb_hd')),]
										},
										{
														# Удалить dstr_hd1 и dstr_adm_hd
														x <- x[!(x$variable %in% getvarlab('dstr_hd1','dstr_adm_hd')),]
										})
										return(x)
})

verb('Удаляем данные по несуществующим волостным депутатам')
# Если в vlst нет слова волость, то удаляем vlst_assmb_dpt. Ещё Полистовское и ...
# datacounts <- datacounts[!(!str_detect(datacounts$vlst, 'волость$|Полистовское') & datacounts$variable==getvarlab('vlst_assmb_dpt')),]
datacounts$freq[(!str_detect(datacounts$vlst, 'волость$|Полистовское') & datacounts$variable==getvarlab('vlst_assmb_dpt'))] <- NA

verb('Удаляем гордуму везде, кроме Пск.Луки')
# datacounts <- datacounts[!(!str_detect(datacounts$district, '^Псков$|Великие Луки') & datacounts$variable == getvarlab('twn_duma')), ]
datacounts$freq[(!str_detect(datacounts$district, '^Псков$|Великие Луки') & datacounts$variable == getvarlab('twn_duma'))] <- NA

verb('Удаляем в Пск.Луки райсобрание')
# datacounts <- datacounts[!(str_detect(datacounts$district, '^Псков$|Великие Луки') & datacounts$variable == getvarlab('dstr_assmb')), ]
datacounts$freq[(str_detect(datacounts$district, '^Псков$|Великие Луки') & datacounts$variable == getvarlab('dstr_assmb'))] <- NA

cat('\nНачинаем считать относительные частоты\n')
datacounts <- getprops(datacounts, 'freq')
cat('\nЗакончили считать относительные частоты\n')

# Список для данных по району.
# Вопрос * районы
# Из таблицы по каждому вопросу/району забрать строчку и отправить в таблицу по вопросу с именем строки - район
# Имя элемента списка - имя вопроса 
districtstotals <- list()

cat("\nГенерируем отчёты по районам\n")
panderOptions('table.split.table', Inf)
d_ply(datacounts, .(district), function(d) Pandoc.brew('report.pdc', output=str_c('/home/sas/1irr/localgov2012/localgov2012/', d$district[1]), open=F))
verb('Генерируем отчёты по полиции')
d_ply(datacounts, .(district), function(d) Pandoc.brew('police.pdc', output=str_c('/home/sas/1irr/localgov2012/localgov2012/police/', d$district[1]), open=F))
# system('sudo /home/sas/1irr/localgov2012/upload.sh')

verb('Досчитываем данные для общеобластного отчёта')
d <- llply(districtstotals, function(x) {
			if (class(x)!='list') return(x)
			as.data.frame(districts2region(x))
			})
verb('Отчёт по области')
Pandoc.brew('district.pdc', output='/home/sas/1irr/localgov2012/localgov2012/Псковская область', open=F)
Pandoc.brew('police_district.pdc', output='/home/sas/1irr/localgov2012/localgov2012/police/Псковская область', open=F)
