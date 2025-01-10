-- Funzione interact che prende in input una funzione che da stringa ritorna una stringa
-- e la applica a tutto il contenuto dello standard input
-- Qundi        mostro la somma dei valori letti
main = interact $ show . sum . map read . words
