p_load('readxl')
dengue <- read.csv("dengue-20151107-big5.csv")
str(dengue) #檢查資料的狀況
summary(dengue)

#把實際的資料篩選出來
filter.idx1 <- dengue$緯度座標 > 22.8 & dengue$緯度座標 < 23.5
filter.idx2 <- dengue$經度座標 > 120 & dengue$經度座標 < 120.6
dengue.tn <- dengue[filter.idx1 & filter.idx2, ]
levels(dengue.tn$區別)

#修正區別的名稱，統一將所有的空白去掉
dengue.tn[dengue.tn$區別 == "北　區", ]$區別 <- "北區"
dengue.tn[dengue.tn$區別 == "東　區", ]$區別 <- "東區"
dengue.tn[dengue.tn$區別 == "南　區" | dengue.tn$區別 == "南    區", ]$區別 <- "南區"
dengue.tn[dengue.tn$區別 == "永康區 ", ]$區別 <- "永康區"

#重新建立一次 factor，這樣可以將空的 levels 去掉
dengue.tn$區別 <- factor(dengue.tn$區別)
#再確認一次區別名稱
levels(dengue.tn$區別)

#畫出每週登革熱的病例數統計圖 breaks=指定分隔好的區間的個數
hist(as.Date(dengue.tn$確診日), breaks = "weeks",
     freq = TRUE, main = "登革熱每週病例數", xlab = "日期",
     ylab = "病例數", format = "%m/%d")

#計算每個月的登革熱病例數：
dengue.tn$month <- format(as.Date(dengue.tn$確診日), "%m")
table(dengue.tn$month)

barplot(table(dengue.tn$month), xlab = "月份", ylab = "病例數",
        main = "登革熱每月病例數")

#計算各個行政區的病例總數 sort()函數是對向量進行從小到大的排序
dengue.region.summary <- sort(summary(dengue.tn$區別), decreasing = FALSE)
dengue.region.summary
#畫圖
barplot(dengue.region.summary, las = 2, horiz = TRUE,
        main = "各行政區病例統計", xlab = "病例數")
pie(dengue.region.summary)

#將最嚴重的五個行政區病例資料篩選出來
dengue.top.reg <- dengue.tn[
  dengue.tn$區別 == "北區" |
    dengue.tn$區別 == "中西區" |
    dengue.tn$區別 == "南區" |
    dengue.tn$區別 == "東區" |
    dengue.tn$區別 == "永康區", ]

#依據時間畫出這 5 個行政區的疫情變化
ggplot(dengue.top.reg, aes(x=as.Date(確診日))) +
  stat_bin(binwidth=7, position="identity") +
  theme(axis.text.x = element_text(angle=90)) +
  xlab("日期") + ylab("病例數") +
  ggtitle("登革熱每週病例數") + facet_grid(區別 ~ .)
