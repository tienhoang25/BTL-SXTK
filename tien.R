
# Cai dat và load packages/libraries
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(dplyr, readr, psych, ggplot2, car)
# Doc du lieu vào Diet
  df_diet <- read.csv("C:/Users/admin/Desktop/Diet.csv")
  head(df_diet) #xem truoc 6 dong dau cua du lieu
# Data cleaning
  # Kiem tra du lieu khuyet NA
  loc <- apply(is.na(df_diet), 2, which)
  loc
  # Xoa du lieu NA
  df_diet <- df_diet[-c(loc$gender),]
  df_diet

# Dieu chinh du lieu
  df_diet <- df_diet %>%
    select(-Person) %>%   
    mutate(weight.lost = pre.weight - weight6weeks,
           Diet = as.factor(Diet)) %>%
    mutate(gender = factor(gender, levels = c("0", "1"),
           labels = c("female", "male")))
    head(df_diet)
    str(df_diet)
# Thong ke mo ta
  # doi voi bien dinh luong
    dinh_luong <- df_diet %>%
      select(-Age, -gender, -Diet)
    stat_dl <- describe(dinh_luong) %>%
      distinct(n, mean, sd, median, max, min) %>%
      mutate(variance = sd^2)
    stat_dl <- t(stat_dl)
    stat_dl
  # V??? so d???
    plot(dinh_luong)
    hist(df_diet$weight.lost, col = "cyan", main = "Weight.lost")
# Doi voi bien dinh tinh
    table(df_diet$gender) # Theo gioi tinh
    table(df_diet$Diet)  # Theo che do an kieng
    
# Ve bieu do boxplot cho bien weight.lost
    boxplot(df_diet$weight.lost ~ df_diet$Diet, xlab = "Diet types",
            ylab = "Weight lost")
    
# Kiem dinh t cho 2 bien pre.weight va weight6weeks
  # Ho: pre.weight > weight6weeks hay che do an kieng hieu qua
  # Ha: pre.weight < weight6weeks hay che do an kieng khong hieu qua
    t.test(df_diet$pre.weight, df_diet$weight6weeks,
    alternative = "less", paired = TRUE, conf.level = 0.95)
    # Do p-value = 1 > 0.05 nên chap nhan gia thuyet Ho
    
# Phan tich phuong sai mot nhan to (one way - ANOVA)
  # Kiem tra su phan phoi chuan (normal distribution)
    # Gia thuyet Ho: Du lieu xap xi phan phoi chuan
    # Gia thuyet Ha: Du lieu khong theo phan phoichuan
      # Phuong phap 1: Kiem tra bang Shapiro wilk test
    shapiro.test(df_diet$weight.lost)
        # Vi p-value > 0.05 : chap nhan gia thuyet Ho
      # Puong phap 2: Kiem tra bang qqplot
    qqnorm(df_diet$weight.lost)
    qqline(df_diet$weight.lost)
        # Qua bieu do, co the xem du lieu phan phoi xap xi phan phoi chuan
    
  # Kiem tra su dong nhat phuong sai
    # Gia thuyet Ho: Phuong sai dong nhat
    # Gia thuyet Ha: Phuong sai khong dong nhat
      leveneTest(weight.lost ~ Diet, df_diet)
      # Ket qua P value > 0.05 nen khong the bac bo Ho
  # Phan tich anova mot nhan to
    # Gia thuyet Ho: khong co su khac nhau ve can nang thay doi giua cac che do an kieng
    # Gia thuyet Ha: co su khac nhau ve can nang thay doi giua cac che do an kieng
    one.way <- aov(weight.lost ~ Diet, data = df_diet)
    summary(one.way)
      # Do p-value < 0.05 nen bac bo Ho, chap nhan Ha, vi vay co su khac biet trong hieu qua
      # cua tung che do an kieng
    # Phan tich so sánh boi
    TukeyHSD(one.way)
    plot(TukeyHSD(one.way), las =1)
    
  # Phan tich anova hai nhan to
    two.way <- aov(weight.lost ~ Diet + gender, data = df_diet)
    summary(two.way)
    # Ket luan: che do an kieng co anh huong den can nang thay doi, con gioi tinh thi ko
    
  # Phan tich su tuong tac giua che do an kieng va gioi tinh
    p<-interaction.plot(df_diet$Diet, df_diet$gender, df_diet$weight.lost,
                     lty = c(1,12),col=c(2:3),lwd = 3,
                     ylab = "mean of weightlost",
                     xlab = "Type of Diet",main="Mean weight lost by diet and gender")
          
    