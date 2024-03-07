# Paket ----
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)

# Tautan ----
tautan_apl_lain <- tags$a(
  shiny::icon("shapes"),
  "Lainnya",
  href = "https://people.usd.ac.id/~ydkristanto/index.php/media-pengajaran/",
  target = "_blank"
)
tautan_github <- tags$a(
  shiny::icon("github"),
  "Github",
  href = "https://github.com/ydkristanto/apl-simulasi-tabungan",
  target = "_blank"
)

# Antarmuka ----
ui <- page_navbar(
  title = "Lempar Undi Dadu",
  id = "lempar_undi_dadu",
  ## Sidebar ----
  sidebar = sidebar(
    ### Sidebar simulasi ----
    conditionalPanel(
      "input.lempar_undi_dadu === 'Simulasi'",
      #### Definisi sukses ----
      selectInput(
        "sukses",
        div("Mata dadu sukses:", style = "font-weight: bold;"),
        choices = c("1", "2", "3", "4", "5", "6"),
        selected = "5",
        multiple = TRUE
      ),
      #### Banyak lempar undi ----
      selectInput(
        "n_lempar",
        div("Banyak lempar undi:", style = "font-weight: bold;"),
        choices = c("1", "10", "100"),
        selected = "1",
        multiple = FALSE
      ),
      #### Pelempar ----
      selectInput(
        "nama_pelempar",
        div("Pelempar undi:", style = "font-weight: bold;"),
        choices = c("Abel", "Ahmad", "Karuna", "Paulina", "Sondang"),
        selected = "Karuna",
        multiple = TRUE,
        selectize = TRUE
      ),
      #### Garis bantu ----
      checkboxInput(
        "garis_bantu",
        "Tampilkan garis bantu",
        value = FALSE
      ),
      hr(),
      #### Tombol eksperimen ----
      actionButton(
        "tombol_eksperimen",
        "Lempar undi!"
      ),
      #### Tombol hapus ----
      actionButton(
        "tombol_hapus",
        "Hapus data"
      )
    ),
    conditionalPanel(
      "input.lempar_undi_dadu === 'Informasi'",
      h4(
        "Deskripsi",
        style = "font-weight: bold; font-size: inherit;"
      ),
      p(
        "Aplikasi Shiny ini mensimulasikan frekuensi relatif suatu kejadian ketika percobaannya semakin banyak."
      ),
      hr(),
      h4(
        "Lisensi MIT",
        style = "font-weight: bold; font-size: inherit;"
      ),
      p(
        "Copyright © 2024 Yosep Dwi Kristanto"
      )
    )
  ),
  ## Simulasi ----
  nav_panel(
    title = "Simulasi",
    icon = icon("dice"),
    navset_card_underline(
      ### Luaran grafik ----
      nav_panel(
        title = "Grafik",
        plotOutput("luaran_grafik")
      ),
      ### Luaran tabel ----
      nav_panel(
        title = "Tabel",
        tableOutput("luaran_tabel")
      )
    )
  ),
  ## Informasi ----
  nav_panel(
    title = "Informasi",
    icon = icon("circle-info"),
    layout_column_wrap(
      width = 1 / 2,
      navset_card_underline(
        #### Tentang ----
        nav_panel(
          title = "Tentang",
          p("Matematika yang terlibat dalam permasalahan deposito dan tabungan berjangka merupakan matematika yang cukup kompleks bagi sebagian besar peserta didik. Aplikasi berbasis web ini menyediakan sarana bagi peserta didik untuk melakukan simulasi terhadap penghitungan deposito dan tabungan berjangka. Dengan demikian, peserta didik diharapkan dapat menemukan ide elementer dari permasalahan deposito dan tabungan berjangka."),
          p("Bagi pendidik, aplikasi ini dapat digunakan untuk memfasilitasi peserta didiknya bermatematika. Aplikasi ini cocok untuk pembelajaran klasikal, penyelidikan dalam kelompok kecil maupun individu. Sebagai saran, pendidik dapat menyiapkan beberapa masalah tentang deposito atau tabungan berjangka sebagai panduan peserta didik untuk melakukan penyelidikan matematis.")
        ),
        nav_panel(
          #### Alat ----
          title = "Alat",
          p("Aplikasi ini dikembangkan dengan menggunakan bahasa pemrograman", a("R", href = "https://www.R-project.org/", target = "_blank"), "dan paket", a("Shiny.", href = "https://CRAN.R-project.org/package=shiny", target = "_blank"), "Paket", a("shinylive", href = "https://posit-dev.github.io/r-shinylive/", target = "_blank"), "digunakan untuk mengekspor aplikasi ini agar dapat dijalankan di peramban web tanpa peladen R yang terpisah. Tata letak dasbor ini diatur dengan menggunakan ", a("bslib.", href = "https://CRAN.R-project.org/package=bslib", target = "_blank"))
        ),
        nav_panel(
          #### Pengembang ----
          title = "Pengembang",
          p("Pengembang dan pemelihara aplikasi ini adalah", a("Yosep Dwi Kristanto,", href = "https://people.usd.ac.id/~ydkristanto/", target = "_blank"), "seorang dosen dan peneliti di program studi", a("Pendidikan Matematika,", href = "https://usd.ac.id/s1pmat", target = "_blank"), a("Universitas Sanata Dharma,", href = "https://www.usd.ac.id/", target = "_blank"), "Yogyakarta.")
        ),
        nav_panel(
          #### Kode Sumber ----
          title = "Kode Sumber",
          p("Kode sumber aplikasi ini tersedia di", a("repositori Github.", href = "https://github.com/ydkristanto/apl-simulasi-tabungan", target = "_blank"), "Jika Anda ingin melaporkan masalah atau meminta fitur tambahan terhadap aplikasi ini, silakan", a("buat sebuah isu", href = "https://github.com/ydkristanto/apl-simulasi-tabungan/issues", target = "_blank"), "atau lebih baik lagi", a("minta penarikan", href = "https://github.com/ydkristanto/apl-simulasi-tabungan/pulls", target = "_blank"), "di repositori tersebut.")
        )
      ),
      #### Matematika ----
      navset_card_underline(
        nav_panel(
          title = "Deposito",
          withMathJax(),
          p("Deposito merupakan tabungan/simpanan yang pencairannya hanya dapat dilakukan pada jangka waktu tertentu. Bunga yang diterima dalam deposito tersebut merupakan bunga majemuk dan penghitungannya dilakukan setiap periode tertentu (umumnya tiap akhir bulan atau 12 kali dalam setahun)."),
          p("Misalkan seorang nasabah memberikan setoran awal sejumlah \\(P\\) kepada bank yang memberikan bunga \\(r\\) per tahunnya dan dihitung \\(n\\) kali setiap tahunnya. Dalam jangka waktu \\(t\\) tahun, jumlah tabungannya dapat dihitung dengan menggunakan rumus berikut."),
          p("$$A(t)=P\\left(1+\\frac{r}{n}\\right)^{nt}$$"),
          p("Ketika penghitungan bunga dilakukan secara kontinu (\\(n\\) mendekati tak hingga), rumusnya menjadi seperti berikut."),
          p("$$A(t)=Pe^{rt}$$dengan \\(e\\) adalah sebuah konstanta matematis yang nilainya kurang lebih sama dengan 2,718282.")
        ),
        nav_panel(
          title = "Tabungan Berjangka",
          p("Tabungan berjangka merupakan tabungan/simpanan yang memungkinkan nasabah menabung secara rutin dalam jangka waktu tertentu. Tabungan berjangka ini merupakan bagian dari ", a("anuitas.", href = "https://kbbi.kemdikbud.go.id/entri/anuitas", target = "_blank"), " Dalam aplikasi ini, tabungan berjangka tersebut menganggap bahwa nasabah melakukan setoran dengan besaran yang sama setiap bulannya."),
          p("Misalnya seorang nasabah melakukan setoran sejumlah \\(R\\) secara rutin sebanyak \\(n\\) kali kepada bank yang memberikan bunga \\(i\\) per periode waktu tertentu. Jumlah tabungan akhirnya dapat ditentukan dengan rumus berikut."),
          p("$$A_f=R\\frac{(1+i)^n-1}{i}$$")
        )
      )
    )
  )
)

# Peladen ----
server <- function(input, output, session) {
  ## Data awal ----
  data_eksperimen <- reactiveVal(
    tibble(
      `Hasil Abel` = integer(),
      `Kategori Abel` = character(),
      `Frek. Rel. Abel` = numeric(),
      `Hasil Ahmad` = integer(),
      `Kategori Ahmad` = character(),
      `Frek. Rel. Ahmad` = numeric(),
      `Hasil Karuna` = integer(),
      `Kategori Karuna` = character(),
      `Frek. Rel. Karuna` = numeric(),
      `Hasil Paulina` = integer(),
      `Kategori Paulina` = character(),
      `Frek. Rel. Paulina` = numeric(),
      `Hasil Sondang` = integer(),
      `Kategori Sondang` = character(),
      `Frek. Rel. Sondang` = numeric()
    )
  )
  
  ## Menambah baris data ----
  observeEvent(input$tombol_eksperimen, {
    # Input
    sukses <- input$sukses
    n_lempar <- input$n_lempar
    
    # Hasil lempar undi setiap anak
    hasil_abel <- sample(
      c(1:6),
      size = n_lempar,
      replace = TRUE
    )
    hasil_ahmad <- sample(
      c(1:6),
      size = n_lempar,
      replace = TRUE
    )
    hasil_karuna <- sample(
      c(1:6),
      size = n_lempar,
      replace = TRUE
    )
    hasil_paulina <- sample(
      c(1:6),
      size = n_lempar,
      replace = TRUE
    )
    hasil_sondang <- sample(
      c(1:6),
      size = n_lempar,
      replace = TRUE
    )
    
    # Menambah data eksperimen
    data_eksperimen() %>% 
      add_row(
        `Hasil Abel` = hasil_abel,
        `Kategori Abel` = ifelse(
          `Hasil Abel` %in% sukses,
          "Sukses", "Gagal"
        ),
        `Frek. Rel. Abel` = rep(NA, n_lempar),
        `Hasil Ahmad` = hasil_ahmad,
        `Kategori Ahmad` = ifelse(
          `Hasil Ahmad` %in% sukses,
          "Sukses", "Gagal"
        ),
        `Frek. Rel. Ahmad` = rep(NA, n_lempar),
        `Hasil Karuna` = hasil_karuna,
        `Kategori Karuna` = ifelse(
          `Hasil Karuna` %in% sukses,
          "Sukses", "Gagal"
        ),
        `Frek. Rel. Karuna` = rep(NA, n_lempar),
        `Hasil Paulina` = hasil_paulina,
        `Kategori Paulina` = ifelse(
          `Hasil Paulina` %in% sukses,
          "Sukses", "Gagal"
        ),
        `Frek. Rel. Paulina` = rep(NA, n_lempar),
        `Hasil Sondang` = hasil_sondang,
        `Kategori Sondang` = ifelse(
          `Hasil Sondang` %in% sukses,
          "Sukses", "Gagal"
        ),
        `Frek. Rel. Sondang` = rep(NA, n_lempar)
      ) %>% data_eksperimen()
  })
  
  ## Menghapus baris data ----
  observeEvent(input$tombol_hapus, {
    data_eksperimen(
      tibble(
        `Hasil Abel` = integer(),
        `Kategori Abel` = character(),
        `Frek. Rel. Abel` = numeric(),
        `Hasil Ahmad` = integer(),
        `Kategori Ahmad` = character(),
        `Frek. Rel. Ahmad` = numeric(),
        `Hasil Karuna` = integer(),
        `Kategori Karuna` = character(),
        `Frek. Rel. Karuna` = numeric(),
        `Hasil Paulina` = integer(),
        `Kategori Paulina` = character(),
        `Frek. Rel. Paulina` = numeric(),
        `Hasil Sondang` = integer(),
        `Kategori Sondang` = character(),
        `Frek. Rel. Sondang` = numeric()
      )
    )
  })
  
  ## Luaran grafik ----
  output$luaran_grafik <- renderPlot({
    # Input
    sukses <- input$sukses
    nama_pelempar <- input$nama_pelempar
    n_baris <- nrow(data_eksperimen())
    
    # Mempersiapkan data
    if (n_baris > 0) {
      data_abel <- data_eksperimen() %>% 
        mutate(
          `Frek. Rel. Abel` = cumsum(`Kategori Abel` == "Sukses") / 
            seq_along(`Hasil Abel`),
          `Frek. Rel. Ahmad` = cumsum(`Kategori Ahmad` == "Sukses") / 
            seq_along(`Hasil Ahmad`),
          `Frek. Rel. Karuna` = cumsum(`Kategori Karuna` == "Sukses") / 
            seq_along(`Hasil Karuna`),
          `Frek. Rel. Paulina` = cumsum(`Kategori Paulina` == "Sukses") / 
            seq_along(`Hasil Paulina`),
          `Frek. Rel. Sondang` = cumsum(`Kategori Sondang` == "Sukses") / 
            seq_along(`Hasil Sondang`)
        ) %>% 
        select(1:3) %>% 
        rename(
          hasil = `Hasil Abel`,
          kategori = `Kategori Abel`,
          fr = `Frek. Rel. Abel`
        ) %>% 
        mutate(
          "nama" = rep("Abel", n_baris),
          "lemparan" = 1:n_baris
        )
      data_ahmad <- data_eksperimen() %>% 
        mutate(
          `Frek. Rel. Abel` = cumsum(`Kategori Abel` == "Sukses") / 
            seq_along(`Hasil Abel`),
          `Frek. Rel. Ahmad` = cumsum(`Kategori Ahmad` == "Sukses") / 
            seq_along(`Hasil Ahmad`),
          `Frek. Rel. Karuna` = cumsum(`Kategori Karuna` == "Sukses") / 
            seq_along(`Hasil Karuna`),
          `Frek. Rel. Paulina` = cumsum(`Kategori Paulina` == "Sukses") / 
            seq_along(`Hasil Paulina`),
          `Frek. Rel. Sondang` = cumsum(`Kategori Sondang` == "Sukses") / 
            seq_along(`Hasil Sondang`)
        ) %>% 
        select(4:6) %>% 
        rename(
          hasil = `Hasil Ahmad`,
          kategori = `Kategori Ahmad`,
          fr = `Frek. Rel. Ahmad`
        ) %>% 
        mutate(
          "nama" = rep("Ahmad", n_baris),
          "lemparan" = 1:n_baris
        )
      data_karuna <- data_eksperimen() %>% 
        mutate(
          `Frek. Rel. Abel` = cumsum(`Kategori Abel` == "Sukses") / 
            seq_along(`Hasil Abel`),
          `Frek. Rel. Ahmad` = cumsum(`Kategori Ahmad` == "Sukses") / 
            seq_along(`Hasil Ahmad`),
          `Frek. Rel. Karuna` = cumsum(`Kategori Karuna` == "Sukses") / 
            seq_along(`Hasil Karuna`),
          `Frek. Rel. Paulina` = cumsum(`Kategori Paulina` == "Sukses") / 
            seq_along(`Hasil Paulina`),
          `Frek. Rel. Sondang` = cumsum(`Kategori Sondang` == "Sukses") / 
            seq_along(`Hasil Sondang`)
        ) %>% 
        select(7:9) %>% 
        rename(
          hasil = `Hasil Karuna`,
          kategori = `Kategori Karuna`,
          fr = `Frek. Rel. Karuna`
        ) %>% 
        mutate(
          "nama" = rep("Karuna", n_baris),
          "lemparan" = 1:n_baris
        )
      data_paulina <- data_eksperimen() %>% 
        mutate(
          `Frek. Rel. Abel` = cumsum(`Kategori Abel` == "Sukses") / 
            seq_along(`Hasil Abel`),
          `Frek. Rel. Ahmad` = cumsum(`Kategori Ahmad` == "Sukses") / 
            seq_along(`Hasil Ahmad`),
          `Frek. Rel. Karuna` = cumsum(`Kategori Karuna` == "Sukses") / 
            seq_along(`Hasil Karuna`),
          `Frek. Rel. Paulina` = cumsum(`Kategori Paulina` == "Sukses") / 
            seq_along(`Hasil Paulina`),
          `Frek. Rel. Sondang` = cumsum(`Kategori Sondang` == "Sukses") / 
            seq_along(`Hasil Sondang`)
        ) %>% 
        select(10:12) %>% 
        rename(
          hasil = `Hasil Paulina`,
          kategori = `Kategori Paulina`,
          fr = `Frek. Rel. Paulina`
        ) %>% 
        mutate(
          "nama" = rep("Paulina", n_baris),
          "lemparan" = 1:n_baris
        )
      data_sondang <- data_eksperimen() %>% 
        mutate(
          `Frek. Rel. Abel` = cumsum(`Kategori Abel` == "Sukses") / 
            seq_along(`Hasil Abel`),
          `Frek. Rel. Ahmad` = cumsum(`Kategori Ahmad` == "Sukses") / 
            seq_along(`Hasil Ahmad`),
          `Frek. Rel. Karuna` = cumsum(`Kategori Karuna` == "Sukses") / 
            seq_along(`Hasil Karuna`),
          `Frek. Rel. Paulina` = cumsum(`Kategori Paulina` == "Sukses") / 
            seq_along(`Hasil Paulina`),
          `Frek. Rel. Sondang` = cumsum(`Kategori Sondang` == "Sukses") / 
            seq_along(`Hasil Sondang`)
        ) %>% 
        select(13:15) %>% 
        rename(
          hasil = `Hasil Sondang`,
          kategori = `Kategori Sondang`,
          fr = `Frek. Rel. Sondang`
        ) %>% 
        mutate(
          "nama" = rep("Sondang", n_baris),
          "lemparan" = 1:n_baris
        )
    } else {
      data_abel <- data_eksperimen() %>% 
        select(1:3)
      data_ahmad <- data_eksperimen() %>% 
        select(4:6)
      data_karuna <- data_eksperimen() %>% 
        select(7:9)
      data_paulina <- data_eksperimen() %>% 
        select(10:12)
      data_sondang <- data_eksperimen() %>% 
        select(13:15)
    }
    
    # Data panjang
    data_panjang <- bind_rows(
      data_abel,
      data_ahmad,
      data_karuna,
      data_paulina,
      data_sondang
    )
    
    # Plot
    if (n_baris > 0) {
      plot <- data_panjang %>% 
        filter(
          nama %in% nama_pelempar
        ) %>% 
        ggplot(aes(x = lemparan, y = fr)) +
        geom_line(
          aes(group = nama, color = nama),
          linewidth = 1.5,
          alpha = .8
        ) +
        geom_point(
          aes(color = nama),
          size = 3,
          alpha = .8
        ) +
        ylim(0, 1) +
        scale_color_viridis_d(
          name = "Nama Pelempar"
        ) +
        theme_bw(base_size = 16) +
        theme(
          legend.position = "bottom",
          plot.title = element_text(face = "bold")
        ) +
        labs(
          title = "Hubungan Antara Nomor Lempar Undi dan Frekuensi Relatif",
          x = "Nomor Lempar Undi",
          y = "Frekuensi Relatif"
        )
      if (input$garis_bantu == TRUE) {
        plot <- plot +
          geom_hline(
            yintercept = length(sukses) / 6,
            linetype = "dashed",
            linewidth = 1
          )
      }
    } else {
      plot <- ggplot(
        data = data.frame(x = as.integer(c(0, 10)), y = c(0, 1)),
        aes(x, y)
      ) +
        geom_blank() +
        theme_bw(
          base_size = 16
        ) +
        theme(
          plot.title = element_text(face = "bold")
        ) +
        labs(
          title = "Hubungan Antara Nomor Lempar Undi dan Frekuensi Relatif",
          x = "Nomor Lempar Undi",
          y = "Frekuensi Relatif"
        )
    }
    
    plot
    
  })
  
  ## Luaran tabel ----
  output$luaran_tabel <- renderTable(
    data_eksperimen() %>% 
      mutate(
        `Frek. Rel. Abel` = cumsum(`Kategori Abel` == "Sukses") / 
          seq_along(`Hasil Abel`),
        `Frek. Rel. Ahmad` = cumsum(`Kategori Ahmad` == "Sukses") / 
          seq_along(`Hasil Ahmad`),
        `Frek. Rel. Karuna` = cumsum(`Kategori Karuna` == "Sukses") / 
          seq_along(`Hasil Karuna`),
        `Frek. Rel. Paulina` = cumsum(`Kategori Paulina` == "Sukses") / 
          seq_along(`Hasil Paulina`),
        `Frek. Rel. Sondang` = cumsum(`Kategori Sondang` == "Sukses") / 
          seq_along(`Hasil Sondang`)
      ) %>% 
      select(
        matches(paste0(input$nama_pelempar, collapse = "|"))
      ),
    striped = TRUE,
    hover = TRUE,
    rownames = TRUE
  )
    
}

# Aplikasi ----
shinyApp(ui = ui, server = server)
