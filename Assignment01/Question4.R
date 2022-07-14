print("Enter a list of numbers then press Enter twice:")
List_num <- scan(what = double())

list_mean <- mean(List_num)
list_median <- median(List_num)
list_sd <- sd(List_num)
list_ra <- max(List_num) - min(List_num)

print(list_mean)
print(list_median)
print(list_sd)
print(list_ra)

