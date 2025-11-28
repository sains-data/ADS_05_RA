---
  title: "TUBES ADS"
author: "Kelompok5"
date: "2025-11-25"
output:
  pdf_document: default
---
  
  ```{r}
library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(effsize)

# --- 1. LOAD DATASET ---

df <- readxl::read_xlsx("Dataset ads clean kelompok 5.xlsx")

# --- 2. BERSIHKAN NAMA KOLOM ---

names(df) <- trimws(names(df))

# --- 3. TEMUKAN KOLOM YANG COCOK ---

org_col <- names(df)[grepl("Organis", names(df), ignore.case = TRUE)]
jam_col <- names(df)[grepl("belajar", names(df), ignore.case = TRUE)]

# --- 4. RENAME SUPAYA MUDAH DIPAKAI ---

df <- df %>%
  rename(
    Organisasi = !!org_col,
    Jam_Belajar = !!jam_col
  )

df$Organisasi_K2 <- ifelse(
  grepl("tidak|akademik", df$Organisasi, ignore.case = TRUE),
  "Aktif",
  "Tidak Aktif"
)

df$Organisasi <- df$Organisasi_K2

df$Jam_Belajar <- as.numeric(df$Jam_Belajar)

# --- 5. TABEL STATISTIK DESKRIPTIF ---

hasil_tabel <- df %>%
  group_by(Organisasi) %>%
  summarise(
    N = n(),
    Mean = mean(Jam_Belajar, na.rm = TRUE),
    SD = sd(Jam_Belajar, na.rm = TRUE),
    Min = min(Jam_Belajar, na.rm = TRUE),
    Q1 = quantile(Jam_Belajar, 0.25, na.rm = TRUE),
    Median = median(Jam_Belajar, na.rm = TRUE),
    Q3 = quantile(Jam_Belajar, 0.75, na.rm = TRUE),
    Max = max(Jam_Belajar, na.rm = TRUE)
  )

# Cetak tabel 

print(hasil_tabel)

# --- 6. VISUALISASI ---
print(
  ggplot(df, aes(x = Organisasi, y = Jam_Belajar, fill = Organisasi)) +
    geom_boxplot() +
    theme_minimal() +
    labs(
      title = "Boxplot Waktu Belajar Berdasarkan Keterlibatan Organisasi",
      x = NULL,  # hapus label sumbu x
      y = "Waktu Belajar (jam/minggu)"
    ) +
    theme(
      axis.text.x = element_blank(),   # hilangkan tulisan di bawah
      axis.ticks.x = element_blank()   # hilangkan garis tick
    )
)

# Histogram
print(
  ggplot(df, aes(x = Jam_Belajar, fill = Organisasi)) +
    geom_histogram(alpha = 0.6, bins = 20) +
    theme_minimal() +
    labs(
      title = "Histogram Waktu Belajar per Kelompok Organisasi",
      x = "Waktu Belajar",
      y = "Frekuensi"
    )
)

# --- 7. VARIABEL 2 KATEGORI ---

df$Organisasi_K2 <- factor(df$Organisasi_K2)

print(table(df$Organisasi_K2))

# --- 8. STATISTIK DESKRIPTIF DUA KELOMPOK ---

tabel_dua_kelompok <- df %>%
  group_by(Organisasi_K2) %>%
  summarise(
    N = n(),
    Mean = mean(Jam_Belajar, na.rm = TRUE),
    SD = sd(Jam_Belajar, na.rm = TRUE),
    Min = min(Jam_Belajar, na.rm = TRUE),
    Median = median(Jam_Belajar, na.rm = TRUE),
    Max = max(Jam_Belajar, na.rm = TRUE)
  )

print(tabel_dua_kelompok)

# --- 9. UJI NORMALITAS ---

norm_aktif <- shapiro.test(df$Jam_Belajar[df$Organisasi_K2 == "Aktif"])
norm_tidak <- shapiro.test(df$Jam_Belajar[df$Organisasi_K2 == "Tidak Aktif"])
norm_aktif
norm_tidak

# --- 10. HOMOGENITAS VARIANS ---

leveneTest(Jam_Belajar ~ Organisasi_K2, data = df)

# --- 11. T-TEST ---

uji_t <- t.test(Jam_Belajar ~ Organisasi_K2, data = df)
uji_t

# --- 12. EFFECT SIZE ---

cohen_d <- cohen.d(df$Jam_Belajar ~ df$Organisasi_K2)
cohen_d

# --- 13. INTERPRETASI OTOMATIS ---

cat("\nINTERPRETASI OTOMATIS:\n")
p_val <- uji_t$p.value
if (p_val < 0.05) {
  cat("Terdapat perbedaan signifikan rata-rata waktu belajar antara mahasiswa aktif dan tidak aktif organisasi.\n")
} else {
  cat("Tidak terdapat perbedaan signifikan rata-rata waktu belajar antara mahasiswa aktif dan tidak aktif organisasi.\n")
}
cat("Nilai p-value: ", p_val, "\n")
cat("Confidence Interval:\n")
print(uji_t$conf.int)

```
