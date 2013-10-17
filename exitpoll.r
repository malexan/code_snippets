library(stringr)
library(plyr)
library(reshape)
library(brew)
library(xtable)
library(RODBC)

#source('d:\\docs\\sas\\irr\\functions\\functions.R')
source('/home/sas/irr/functions/functions.R')

# Функция по извлечению чисел из строки
str_extract_digs <- function(s) {
  exp <- '([0-9]+)'
  #llply(seq_along(s), function(x) {
				charlist <- str_extract_all(s, exp)
				as.numeric(unlist(charlist))
  #})
}  

# Функция по отправке смс
sendsms <- function(phone, text) {
				system(str_c('./sendsms.sh ', phone, ' "', text, '"'))
}

# Функция по запросу исправления
request_correction <- function(phone, error) {
				suffix <- ' Ispravte. Format: UIK yavka Zhir Zyug Mir Proh Put otkazi'
				if (error == 1) text <- 'V vashey sms net chisel.'
				if (error == 2) text <- 'Kod uchastka ne nayden v baze.'
				if (error == 3) text <- 'V vashey sms kolichestvo chisel ne ravno 8.'
				if (error == 4) text <- 'V vashey sms yavka menshe summi oproshennih i otkazov.'
				sendsms(phone, str_c(text, suffix))

}

# Функция по предупреждению бригадира о повторной ошибке интера
# Типа, интер с такой-то УИК два раза ошибся 
alert_dm <- function(station, hour, phone) {
				text <- str_c('UIK#', station, ': za ', hour, '-y chas oshibka v sms 2-y raz. Nuzhna pravilnaya sms do ', hour + 1,':40. Esli pozzhe, to golosom na 89532319631')
				sendsms(phone, text)
}

# Функция по предупреждению бригадира о тишине интера
alert_silence <- function(station, hour, phone) {
				text <- str_c('UIK#', station, ': za proshedshiy chas ne bilo sms. Nuzhna sms do ', hour + 1,':40. Esli pozzhe, to golosom na 89532319631')
				sendsms(phone, text)
}

# Функция по поиску молчащих УИКов
check_silence <- function(acceptor='inter') {
				z <- as.POSIXlt(Sys.time())
				z <- unlist(z)
				hour <- z['hour']
				for (i in okrugs$station) {
								if (is.na(stmon[str_c('s', i), str_c('h', hour-1)])) {
												phone <- okrugs$phone[okrugs$station==i]
												alert_silence(i, hour -1, phone)
								}
				}
}
								


# Функция по генерации тестового массива raw
# аргументы: типы ошибок
create_fake_raw <- function(size = 150, date = '2012-03-04', hour = 9, firstsmsid=4000) {
  # Параметры генерации
	pvoices <- c(.08, .15, .07, .10, .60)
  prefusal <- .25
	to <- .59
	stationsize <- 2000
	hour <- 9
	step <- 3
	err <- .0
	cands <- c('Жириновский', 'Зюганов', 'Миронов', 'Прохоров', 'Путин')
	# таблица по округам  
	# sqlSave(ep12db, okrugs, tablename='okrugs', append=F, rownames=F)
	#rnorm
	# Функция по созданию одного сообщения
	makefakesms <- function(station, smsid) {
    # Явка (размер участка делим на кол-во часов работы и умн. на прогноз явки)
		voters <- round(rnorm(1, stationsize/12*to, 20)) 
	  # Кол-во отказов 
    refusal <- round(rnorm(1, voters * prefusal, voters * prefusal * .3))

    # Голоса за кандидатов
	  # Добавляем exclude=NULL чтобы не пропадали кандидаты, которых не выбрали,
	  # а потом удаляем лишний ноль, который NA
    voices <- as.numeric(table(sample(factor(cands), voters/step, T, pvoices)))
    # Номер псевдотелефона
		sender <- paste('7953231', station, sep='')
    # Текст смс-сообщения (номер УИК, явка, голоса)
		text <- paste(station, paste(voters, paste(voices, collapse=' '), refusal, sep=' '))
	  # Дата|время прихода сообщения
		date <- format(as.POSIXct.numeric(1330844400, origin='1970-01-01') + rnorm(1, 3600*(hour - 7) +300, 300), "%d-%m-%Y %H:%M")
    # Добавляем кавычки, точки с зяпятой, как в оригинале у башни
		sms <- paste(smsid, text, date, sender, 7777777777, sep=';')
	  sms <- str_replace_all(sms, ";", "\";\"")
	  sms <- str_replace(sms, '^', '\"')
	  sms <- str_replace(sms, '$', '\";')
    
		# Ошибка в смс (пропавший пробел)
    # Будет ли ошибка (вероятность err)
		if (sample(0:1, 1, prob=c(err, 1 - err))==0) {
		  # Положение пробелов в тексте смс
		  # -1 чтобы не учитывать пробел в дате
			spacespos <- str_locate_all(sms, ' ')[[1]][,1]
		  spacespos <- spacespos[-length(spacespos)]
		  # Какой пробел будет пропущен?
			spacen <- sample(spacespos, 1)
			# Удаляем пробел
			sms <- str_c(str_sub(sms, end=spacen - 1), str_sub(sms, spacen + 1))
		}
    return(sms)
	# Конец функции по ген-ции 1 смски	
  }

	# код смс, передача часа
	newdata <- laply(okrugs$station, makefakesms, 5570)
	cat((laply(okrugs$station, makefakesms, 5570)), file='data.csv', append=T,  sep='\n')
	newdata <- newdata[sample(length(newdata), size = (1 - err) * length(newdata))]
	return(newdata)
}


# Функция по сбросу всех данных для периода тестирования
reset_all <- function(doit=F) {
				if (doit==F) return(1)
				# команда для сохранения первичного массива индексов старых смс
				realrawids <- read.table('old_ids')
				colnames(realrawids) <- 'id'
				sqlDrop(ep12db, 'old', errors=FALSE)
				sqlSave(ep12db, realrawids, tablename='old', append=F, rownames=F)
				# создание пустого массива для мониторинга 
				stmon <- array(dim=c(length(okrugs$station), 20-8)) 
				colnames(stmon) <- str_c('h', 8:19)
				rownames(stmon) <- str_c('s', okrugs$station)
				sqlDrop(ep12db, 'data', errors=FALSE)
				sqlDrop(ep12db, 'stmon', errors=FALSE)
				sqlSave(ep12db, as.data.frame(stmon), tablename='stmon', append=F, rownames=T)
				return(0)
}



# Проверка смс на соответствие нашим требованиям
# Функция возращает следующие коды:
# 0 - ок, можно добавлять в массив;
# 1 - ошибка "Нет чисел в смс"
# 2 - ошибка "Несуществующий номер УИК"
# 3 - ошибка "Количество чисел не совпадает с базой"
# 4 - ошибка "Явка меньше числа опрошенных"
# Предупреждение 1 - "Распределение голосов значимо отличается от прогноза" 
# Предупреждение 2 - "Распределение голосов значимо отличается от предыдущих часов" 
test_sms <- function(smstext) {
  digs <- str_extract_digs(smstext)
  if (length(digs)==0) return(1)
	if (!(digs[1] %in% okrugs$station)) return(2)
	if (length(digs)!=8) return(3)
	if (digs[2] < sum(digs[3:8])) return(4)
	return(0)
}

# Преобразование времени прихода смс в час
# Если сообщение приходит до дедлайна - оно помечается текущим часом;
# если в дедлайн и после, - то следующим часом;
# если интервьюер не успевает прислать сообщение за отчётный час
# до дедайлна след.часа, то ОБЪЕДИНИТЬ ДАННЫЕ или НЕ ПРИСЫЛАТЬ
get_hour <- function(x, ft='%d-%m-%Y %H:%M', deadline=50) {
				x <- as.POSIXlt(x, format=ft)
				x <- unclass(x)
				hour <- ifelse(x$min>deadline, x$hour, x$hour - 1)
				return(hour)
}

# Функция по обновлению статуса УИКа
# Если предыдущий статус был пустым, то ставим имеющ.значение
# если был ноль, а смс опять хорошая - ОБРАБОТАТЬ
# если была ошибка, а смс хорошая - то заменяем на 0
# если была ошибка и опять ошибка, то 5 
# ПРОТЕСТИТЬ с непустым stmon
new_status <- function(x) {
				curr_state <- x$test
				prev_state <- stmon[str_c('s', x$station), str_c('h', x$hour)]
				new_state <- NA
				if (curr_state==0) new_state <- 0
				if (is.na(prev_state) & curr_state>0) new_state <- curr_state
				if (!is.na(prev_state) & curr_state>0) new_state <- 5
 				if (is.na(new_state)) new_state <- 6
				return(new_state)
}


# Функция прикладыванию к округам столбика с именами бригадиров
# чтобы можно было отправлять бригадиру смс в случае повторной ошибки
# интера с этого УИКа 
join_okrugs_sv <- function(doit=T) {
				# получаем массив с округами
				okrugs <- sqlFetch(ep12db, 'okrugs')
				sv <- sqlFetch(ep12db, 'superviser')
				sv_okrugs <- sqlFetch(ep12db, 'sv_okrugs')
				# добавляем два уровня для выпендрившихся бригадиров        
				sv_okrugs$superviser <- factor(sv_okrugs$superviser, levels 
																			 = c(levels(sv_okrugs$superviser), 'Вредова',
																					 'Папишина'))
				okrugs <- join(okrugs, sv_okrugs)
				okrugs$superviser[okrugs$okrug %in% 4:6] <- 'Вредова'
				okrugs$superviser[okrugs$okrug %in% 1:3] <- 'Папишина'
				okrugs <- join(okrugs, sv)
				return(okrugs)
}

# Функция для оперативного отображения статуса УИКов по бригадирам.
get_stmon_by_sv <- function(sv) {
				stmon[str_c('s', okrugs$station[okrugs$superviser==sv]),]
}

# Подключаем базу данных
ep12db <- odbcConnect('ep12')

# получаем массив с округами, УИКами, бригадирами, их телефонами
okrugs <- join_okrugs_sv()


# Вызов скрипта, получающего данные с башни
system('./getcsv.sh')

# Пишем айдишники старных настоящих смс в файл для функции reset_all()
# raw <- read.table('data.csv', sep=';', quote='"', stringsAsFactors = F, col.names = c('id', 'text', 'date', 'receiver', 'sender', 'eof'), fileEncoding='cp1251')
# write.table(raw$id, file='old_ids', row.names=F, col.names=F, quote=F)

# Добавляем к полученному файлу фейк
# cat(create_fake_raw(), file='data.csv', append = T, sep = '\n')

# Читаем все полученные смс включая фейк, если был
raw <- read.table('data.csv', sep=';', quote='"', stringsAsFactors = F, 
col.names = c('id', 'text', 'date', 'receiver', 'sender', 'eof'), fileEncoding='cp1251')

# Удаляем старые сообщения 
# получаем массив с индексами старых переменных 
oldindex <- sqlFetch(ep12db, 'old')
oldindex <- oldindex$id

#собственно, удаляем
newraw <- raw[!raw$id %in% oldindex, ]

# Удаляем получателя и последний пустой столбец
newraw[['receiver']] <- NULL
newraw[['eof']] <- NULL

#Прерываем выполнение, если новых данных не поступило
if (dim(newraw)[1] < 1) stop("Нет новых смс")


newraw$test <- laply(newraw$text, test_sms)

# отправка смс по ошибкам «нет чисел» и «неправильный уик»
unidentified <- newraw[(newraw$test %in% 1:2), ]
for (i in seq_along(unidentified$id)) {
				request_correction(unidentified$sender[i], unidentified$test[i])
}

# оставляем смс с идентифицируемыми уиками 
newdata <- newraw[(newraw$test %in% c(0, 3:4)), ]
faststationstatus <- cbind(newdata[c(-2, -3)], laply(newdata$text, function(x) str_extract_digs(x)[1]), laply(newdata$date, get_hour))
names(faststationstatus)[4:5] <- c('station', 'hour')

# читаем мониторинг УИК
stmon <- sqlFetch(ep12db, 'stmon')

# добавляем колонку с настоящим статусом
faststationstatus <- adply(faststationstatus, .margins=1, new_status)

#отправка смс по ошибкам «кол-во чисел» и «явка и сумма» и "ошибка второй раз"
a_ply(faststationstatus, .margins=1, function(x) {
			if(x$V1 %in% 3:4) request_correction(x$sender, x$V1)
		  if(x$V1 == 5) alert_dm(x$station, x$hour,  okrugs$phone[okrugs$station==x$station])
				}	
			)

# обновляем таблицу с мониторингом уиков
for (i in seq_along(faststationstatus$station)) {
				stmon[str_c('s', faststationstatus$station[i]), str_c('h', faststationstatus$hour[i])] <- faststationstatus$V1[i]
}

# пишем таблицу с мониторингом в базу
sqlDrop(ep12db, 'stmon', errors=F)
sqlSave(ep12db, stmon)


# удаляем из массива смс с ошибками
newdata <- newdata[newdata$test==0, ]
newdata <- cbind(newdata, ldply(newdata$text, str_extract_digs), ldply(newdata$date, get_hour))
newdata$text <- NULL
newdata$test <- NULL
colnames(newdata) <- c('id', 'date', 'sender', 'station', 'to', str_c('c', 1:5), 'refusal', 'hour')
sqlSave(ep12db, newdata, tablename='data', rownames=F, append=T)

# Обновляем массив с номерами отюзанных смс
sqlSave(ep12db, newraw['id'], tablename='old', append=T, rownames=F)


# получаем обновлённый массив
data <- sqlFetch(ep12db, 'data')
# добавляем в данные инфу по географии
data <- join(data, okrugs)

source('preparedata.R')

close(ep12db)
