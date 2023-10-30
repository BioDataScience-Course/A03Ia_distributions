# Vérifications de zooplankton_notebook.qmd
zoo <- parse_rmd("../../zooplankton_notebook.qmd",
  allow_incomplete = TRUE, parse_yaml = TRUE)

test_that("Le bloc-notes est-il compilé en un fichier final HTML ?", {
  expect_true(is_rendered("zooplankton_notebook.qmd"))
  # La version compilée HTML du carnet de notes est introuvable
  # Vous devez créer un rendu de votre bloc-notes Quarto (bouton 'Rendu')
  # Vérifiez aussi que ce rendu se réalise sans erreur, sinon, lisez le message
  # qui s'affiche dans l'onglet 'Travaux' et corrigez ce qui ne va pas dans
  # votre document avant de réaliser à nouveau un rendu HTML.
  # IL EST TRES IMPORTANT QUE VOTRE DOCUMENT COMPILE ! C'est tout de même le but
  # de votre analyse que d'obtenir le document final HTML.

  expect_true(is_rendered_current("zooplankton_notebook.qmd"))
  # La version compilée HTML du document Quarto existe, mais elle est ancienne
  # Vous avez modifié le document Quarto après avoir réalisé le rendu.
  # La version finale HTML n'est sans doute pas à jour. Recompilez la dernière
  # version de votre bloc-notes en cliquant sur le bouton 'Rendu' et vérifiez
  # que la conversion se fait sans erreur. Sinon, corrigez et regénérez le HTML.
})

test_that("La structure du document est-elle conservée ?", {
  expect_true(all(c("Introduction et but", "Matériel et méthodes",
    "Résultats", "Histogrammes", "Graphiques de densité",
    "Graphiques en violon et en lignes de crête",
    "Discussion et conclusion", "Références")
    %in% (rmd_node_sections(zoo) |> unlist() |> unique())))
  # Les sections (titres) attendues du bloc-notes ne sont pas toutes présentes
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs titres indispensables par rapport aux exercices ont disparu ou ont
  # été modifié. Vérifiez la structure du document par rapport à la version
  # d'origine dans le dépôt "template" du document (lien au début du fichier
  # README.md).

  expect_true(all(c("setup", "import", "names", "zoo_sub", "histo1a", "histo1b",
    "histo1c", "histo1d", "histo1comment", "histo1comment2", "histo2", "histo3",
    "histo2comment", "density1", "density1comment", "density2",
    "density2comment", "violin", "ridges", "violincomment")
    %in% rmd_node_label(zoo)))
  # Un ou plusieurs labels de chunks nécessaires à l'évaluation manquent
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs chunks indispensables par rapport aux exercices sont introuvables.
  # Vérifiez la structure du document par rapport à la version d'origine dans
  # le dépôt "template" du document (lien au début du fichier README.md).

  expect_true(any(duplicated(rmd_node_label(zoo))))
  # Un ou plusieurs labels de chunks sont dupliqués
  # Les labels de chunks doivent absolument être uniques. Vous ne pouvez pas
  # avoir deux chunks qui portent le même label. Vérifiez et modifiez le label
  # dupliqué pour respecter cette règle. Comme les chunks et leurs labels sont
  # imposés dans ce document cadré, cette situation ne devrait pas se produire.
  # Vous avez peut-être involontairement dupliqué une partie du document ?
})

test_that("L'entête YAML a-t-il été complété ?", {
  expect_true(zoo[[1]]$author != "___")
  expect_true(!grepl("__", zoo[[1]]$author))
  expect_true(grepl("^[^_]....+", zoo[[1]]$author))
  # Le nom d'auteur n'est pas complété ou de manière incorrecte dans l'entête
  # Vous devez indiquer votre nom dans l'entête YAML à la place de "___" et
  # éliminer les caractères '_' par la même occasion.

  expect_true(grepl("[a-z]", zoo[[1]]$author))
  # Aucune lettre minuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en majuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.

  expect_true(grepl("[A-Z]", zoo[[1]]$author))
  # Aucune lettre majuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en minuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.
})

test_that("Chunk 'import' : importation des données", {
  expect_true(is_identical_to_ref("import", "names"))
  # Les colonnes dans le tableau `zoo` importé ne sont pas celles attendues
  # Votre jeu de données de départ n'est pas correct. Ce test échoue si vous
  # n'avez pas bien rempli le code du chunk 'import'.

  expect_true(is_identical_to_ref("import", "classes"))
  # La nature des variables (classe) dans le tableau `zoo` est incorrecte
  # Vérifiez le chunk d'importation des données `import`.

  expect_true(is_identical_to_ref("import", "nrow"))
  # Le nombre de lignes dans le tableau `zoo` est incorrect
  # Vérifiez l'importation des données dans le chunk d'importation `import` et
  # réexécutez-le pour corriger le problème.
})

test_that("Chunk 'zoo_sub' : données restreintes aux copépodes", {
  expect_true(is_identical_to_ref("zoo_sub", "names"))
  # Les colonnes dans le tableau `zoo_sub` ne sont pas celles attendues
  # Votre jeu de données filtré n'est pas correct. Ce test échoue si vous
  # avez modifié le code du chunk 'zoo_sub' (il était prérempli).

  expect_true(is_identical_to_ref("zoo_sub", "classes"))
  # La nature des variables (classe) dans le tableau `zoo_sub` est incorrecte
  # Vérifiez le chunk `zoo_sub` (il ne fallait rien y modifier).

  expect_true(is_identical_to_ref("zoo_sub", "nrow"))
  # Le nombre de lignes dans le tableau `zoo_sub` est incorrect
  # Vérifiez le filtrage des données dans le chunk `zoo_sub` et réexécutez-le
  # pour corriger le problème (il ne fallait rien y modifier).
})

test_that("Chunks 'histo1a-d' & 'histo1comment', 'histo1comment2' : histogramme de ECD", {
  expect_true(is_identical_to_ref("histo1a"))
  # L'histogramme produit par 'histo1a' n'est pas celui attendu
  # Lisez bien la consigne et corrigez l'erreur, puis refaites un 'Rendu' du
  # document avant de retester.

  expect_true(is_identical_to_ref("histo1b"))
  # L'histogramme produit par 'histo1b' n'est pas celui attendu
  # Lisez bien la consigne et corrigez l'erreur, puis refaites un 'Rendu' du
  # document avant de retester

  expect_true(is_identical_to_ref("histo1c"))
  # L'histogramme produit par 'histo1c' n'est pas celui attendu
  # Lisez bien la consigne et corrigez l'erreur, puis refaites un 'Rendu' du
  # document avant de retester.

  expect_true(is_identical_to_ref("histo1d"))
  # L'histogramme produit par 'histo1d' n'est pas celui attendu
  # Lisez bien la consigne et corrigez l'erreur, puis refaites un 'Rendu' du
  # document avant de retester.

  expect_true(is_identical_to_ref("histo1comment"))
  # Vous n'avez pas choisi le bon histogramme
  # Vous devez cochez l'item qui correspond au meilleur histogramme d'un 'x'
  # entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML
  # du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !

  expect_true(is_identical_to_ref("histo1comment2"))
  # L'interprétation de l'histogramme est (partiellement) fausse
  # Vous devez cochez les phrases qui décrivent le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'histo2', 'histo3' & 'histo2comment' : histogrammes de l'ECD des copépodes", {
  expect_true(is_identical_to_ref("histo2"))
  # L'histogramme produit par 'histo2' n'est pas celui attendu
  # Lisez bien la consigne et corrigez l'erreur, puis refaites un 'Rendu' du
  # document avant de retester.

  expect_true(is_identical_to_ref("histo3"))
  # L'histogramme produit par 'histo3' n'est pas celui attendu
  # Lisez bien la consigne et corrigez l'erreur, puis refaites un 'Rendu' du
  # document avant de retester

  expect_true(is_identical_to_ref("histo2comment"))
  # L'interprétation des histogrammes de l'ECD des copépodes est (partiellement)
  # fausse
  # Vous devez cochez les phrases qui décrivent le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'density1' & 'density1comment' : graphique de densité de la D.O. moyenne", {
  expect_true(is_identical_to_ref("density1"))
  # Le graphique de densité produit par 'density1' n'est pas celui attendu
  # Lisez bien la consigne et corrigez l'erreur, puis refaites un 'Rendu' du
  # document avant de retester.

  expect_true(is_identical_to_ref("density1comment"))
  # L'interprétation du premier graphique de densité est (partiellement)
  # fausse
  # Vous devez cochez les phrases qui décrivent le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'density2' & 'density2comment' : graphique de densité de la D.O. max", {
  expect_true(is_identical_to_ref("density2"))
  # Le graphique de densité produit par 'density2' n'est pas celui attendu
  # Lisez bien la consigne et corrigez l'erreur, puis refaites un 'Rendu' du
  # document avant de retester.

  expect_true(is_identical_to_ref("density2comment"))
  # L'interprétation du second graphique de densité est (partiellement)
  # fausse
  # Vous devez cochez les phrases qui décrivent le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'violin', 'ridges' & 'violincomment' : graphiques en violon et en lignes de crête", {
  expect_true(is_identical_to_ref("violin"))
  # Le graphique en violon produit par 'violin' n'est pas celui attendu
  # Lisez bien la consigne et corrigez l'erreur, puis refaites un 'Rendu' du
  # document avant de retester.

  expect_true(is_identical_to_ref("ridges"))
  # Le graphique en lignes de crête produit par 'ridges' n'est pas celui attendu
  # Lisez bien la consigne et corrigez l'erreur, puis refaites un 'Rendu' du
  # document avant de retester.

  expect_true(is_identical_to_ref("violincomment"))
  # L'interprétation des graphiques en violon et en lignes de crête est
  # (partiellement) fausse
  # Vous devez cochez les phrases qui décrivent le graphique d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})
