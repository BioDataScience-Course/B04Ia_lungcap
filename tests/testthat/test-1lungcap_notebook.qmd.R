# Vérifications de lungcap_notebook.qmd
lungcap <- parse_rmd("../../lungcap_notebook.qmd",
  allow_incomplete = TRUE, parse_yaml = TRUE)

test_that("Le bloc-notes est-il compilé en un fichier final HTML ?", {
  expect_true(is_rendered("lungcap_notebook.qmd"))
  # La version compilée HTML du carnet de notes est introuvable
  # Vous devez créer un rendu de votre bloc-notes Quarto (bouton 'Rendu')
  # Vérifiez aussi que ce rendu se réalise sans erreur, sinon, lisez le message
  # qui s'affiche dans l'onglet 'Travaux' et corrigez ce qui ne va pas dans
  # votre document avant de réaliser à nouveau un rendu HTML.
  # IL EST TRES IMPORTANT QUE VOTRE DOCUMENT COMPILE ! C'est tout de même le but
  # de votre analyse que d'obtenir le document final HTML.

  expect_true(is_rendered_current("lungcap_notebook.qmd"))
  # La version compilée HTML du document Quarto existe, mais elle est ancienne
  # Vous avez modifié le document Quarto après avoir réalisé le rendu.
  # La version finale HTML n'est sans doute pas à jour. Recompilez la dernière
  # version de votre bloc-notes en cliquant sur le bouton 'Rendu' et vérifiez
  # que la conversion se fait sans erreur. Sinon, corrigez et regénérez le HTML.
})

test_that("La structure du document est-elle conservée ?", {
  expect_true(all(c("Introduction et but", "Matériel et méthodes",
    "Résultats", "Étude descriptive des données", "Volume respiratoire maximal",
    "Probabilité d'être fumeur", "Discussion et conclusion", "Références")
    %in% (rmd_node_sections(lungcap) |> unlist() |> unique())))
  # Les sections (titres) attendues du bloc-notes ne sont pas toutes présentes
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs titres indispensables par rapport aux exercices ont disparu ou ont
  # été modifié. Vérifiez la structure du document par rapport à la version
  # d'origine dans le dépôt "template" du document (lien au début du fichier
  # README.md).

  expect_true(all(c("setup", "import", "importcomment", "table", "corr",
    "fevht", "fevage", "desccomment", "fev_glm", "fev_glmcomment", "fev_glm2",
    "fev_glm2comment", "fev_compa", "fev_compacomment", "fev_resid",
    "fev_residcomment", "fev_lm", "fev_lmcomment", "smoke_glm",
    "smoke_glmcomment", "smoke_glm2", "smoke_compa", "smoke_compacomment",
    "smoke_glm3", "smoke_glm3comment")
    %in% rmd_node_label(lungcap)))
  # Un ou plusieurs labels de chunks nécessaires à l'évaluation manquent
  # Ce test échoue si vous avez modifié la structure du document, un ou
  # plusieurs chunks indispensables par rapport aux exercices sont introuvables.
  # Vérifiez la structure du document par rapport à la version d'origine dans
  # le dépôt "template" du document (lien au début du fichier README.md).

  expect_true(any(duplicated(rmd_node_label(lungcap))))
  # Un ou plusieurs labels de chunks sont dupliqués
  # Les labels de chunks doivent absolument être uniques. Vous ne pouvez pas
  # avoir deux chunks qui portent le même label. Vérifiez et modifiez le label
  # dupliqué pour respecter cette règle. Comme les chunks et leurs labels sont
  # imposés dans ce document cadré, cette situation ne devrait pas se produire.
  # Vous avez peut-être involontairement dupliqué une partie du document ?
})

test_that("L'entête YAML a-t-il été complété ?", {
  expect_true(lungcap[[1]]$author != "___")
  expect_true(!grepl("__", lungcap[[1]]$author))
  expect_true(grepl("^[^_]....+", lungcap[[1]]$author))
  # Le nom d'auteur n'est pas complété ou de manière incorrecte dans l'entête
  # Vous devez indiquer votre nom dans l'entête YAML à la place de "___" et
  # éliminer les caractères '_' par la même occasion.

  expect_true(grepl("[a-z]", lungcap[[1]]$author))
  # Aucune lettre minuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en majuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.

  expect_true(grepl("[A-Z]", lungcap[[1]]$author))
  # Aucune lettre majuscule n'est trouvée dans le nom d'auteur
  # Avez-vous bien complété le champ 'author' dans l'entête YAML ?
  # Vous ne pouvez pas écrire votre nom tout en minuscules. Utilisez une
  # majuscule en début de nom et de prénom, et des minuscules ensuite.
})

test_that("Chunks 'import' & 'importcomment' : importation des données", {
  expect_true(is_identical_to_ref("import", "names"))
  # Les colonnes dans le tableau `lungcap` importé ne sont pas celles attendues
  # Votre jeu de données de départ n'est pas correct. Ce test échoue si vous
  # n'avez pas bien rempli le code du chunk 'import'.

  expect_true(is_identical_to_ref("import", "classes"))
  # La nature des variables (classe) dans le tableau `lungcap` est incorrecte
  # Vérifiez le chunk d'importation des données `import`.

  expect_true(is_identical_to_ref("import", "nrow"))
  # Le nombre de lignes dans le tableau `lungcap` est incorrect
  # Vérifiez l'importation des données dans le chunk d'importation `import` et
  # réexécutez-le pour corriger le problème.

  expect_true(is_identical_to_ref("importcomment"))
  # L'interprétation de la description des données est (partiellement) fausse
  # dans 'importcomment'
  # Vous devez cochez les phrases qui décrivent les tables descriptives d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'table', 'corr', fevht', fevage' & 'desccomment' : description des données", {
  expect_true(is_identical_to_ref("table"))
  # Le tableau de contingence produit par le chunk 'table' n'est pas celui
  # attendu
  # Lisez bien la consigne et corrigez l'erreur. Si le test précédent
  # d'importation est incorrect, celui-ci l'est aussi automatiquement. Sinon,
  # vérifiez le code du chunk et corrigez-le.

  expect_true(is_identical_to_ref("corr"))
  # Le graphique des corrélations produit par le chunk 'corr' n'est pas celui
  # attendu
  # Lisez bien la consigne et corrigez l'erreur. Sélectionnez les cinq variables
  # quantitatives dans l'ordre d'apparition dans le tableau.

  expect_true(is_identical_to_ref("fevht"))
  # Le graphique produit par le chunk 'fevht' n'est pas celui attendu
  # Lisez bien la consigne et corrigez l'erreur. Il vous faut un graphique en
  # nuage de points avec deux variables quantitatives, une variation de couleur
  # par 'année'Smoke' et des facettes pour la variable 'Gender'.

  expect_true(is_identical_to_ref("fevage"))
  # Le graphique produit par le chunk 'fevage' n'est pas celui attendu
  # Lisez bien la consigne et corrigez l'erreur. Il vous faut un graphique en
  # nuage de points avec deux variables quantitatives, une variation de couleur
  # par 'année'Smoke' et des facettes pour la variable 'Gender'.

  expect_true(is_identical_to_ref("desccomment"))
  # L'interprétation de la description des données est (partiellement) fausse
  # dans le chunk 'desccomment'
  # Vous devez cochez les phrases qui décrivent les graphiques et la table d'un
  # 'x' entre les crochets [] -> [x]. Ensuite, vous devez recompiler la version
  # HTML du bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les
  # résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'fev_glm', 'fev_glmcomment', premier modèle linéaire généralisé pour FEV", {
  expect_true(is_identical_to_ref("fev_glm"))
  # Le premier modèle linéaire généralisé pour FEV (chunk 'fev_glm') n'est pas
  # le bon
  # Vérifiez en particulier la formule que vous avez écrite pour décrire la
  # relation entre les variables dans votre modèle. Relisez les consignes
  # attentivement, si nécessaire.

  expect_true(is_identical_to_ref("fev_glmcomment"))
  # L'interprétation du premier modèle linéaire généralisé pour FEV est
  # (partiellement) fausse
  # Vous devez cochez les phrases qui décrivent le modèle d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'fev_glm2', 'fev_glm2comment', premier modèle linéaire généralisé pour FEV", {
  expect_true(is_identical_to_ref("fev_glm2"))
  # Le second modèle linéaire généralisé pour FEV (chunk 'fev_glm') n'est pas
  # le bon
  # Vérifiez en particulier la formule que vous avez écrite pour décrire la
  # relation entre les variables dans votre modèle. Relisez les consignes
  # attentivement, si nécessaire.

  expect_true(is_identical_to_ref("fev_glm2comment"))
  # L'interprétation du second modèle linéaire généralisé pour FEV est
  # (partiellement) fausse
  # Vous devez cochez les phrases qui décrivent le modèle d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'fev_compa', 'fev_compacomment', comparaison des deux premiers modèles généralisés pour FEV", {
  expect_true(is_identical_to_ref("fev_compa"))
  # La comparaison des deux premiers modèles généralisés pour FEV (chunk
  # 'compa') n'est pas correcte
  # Vérifiez votre code et indiquez les modèles dans l'ordre comme arguments.
  # Vous devez aussi préciser que vous voulez un test "F".

  expect_true(is_identical_to_ref("fev_compacomment"))
  # L'interprétation de la comparaison des deux premiers modèles généralisés
  # pour FEV est (partiellement) fausse
  # Vous devez cochez les phrases qui décrivent la comparaison d'un 'x' entre
  # les crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'fev_resid' & 'fev_residcomment' : graphiques d'analyse des résidus pour le modèle FEV", {
  expect_true(is_identical_to_ref("fev_resid"))
  # Les graphiques d'analyse des résidus du modèle FEV ne sont pas réalisé ou
  # sont incorrects
  # Relisez les consignes et vérifiez votre code concernant ce graphique.

  expect_true(is_identical_to_ref("fev_residcomment"))
  # L'interprétation des graphiques d'analyse des résidus du modèle FEV est
  # (partiellement) fausse
  # Vous devez cochez les phrases qui décrivent les graphiques d'un 'x' entre
  # les crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Le code pour l'équation paramétrée du modèle FEV est-il correct ?", {
  expect_true(rmd_select(lungcap, by_section(
    "Volume respiratoire maximal")) |>
      as_document() |> is_display_param_equation("fev_glm2"))
  # Le code pour générer l'équation paramétrée du modèle pour FEV est incorrect
  # Vous avez appris à faire cela dans un learnr du module 2. Revoyez cette
  # matière et vérifiez comment l'équation se présente dans le document final
  # généré avec le bouton ('Rendu').
})

test_that("Chunks 'fev_lm', 'fev_lmcomment', modèle linéaire pour FEV", {
  expect_true(is_identical_to_ref("fev_lm"))
  # Le modèle linéaire pour FEV (chunk 'fev_lm') n'est pas le bon
  # Vérifiez en particulier la formule que vous avez écrite pour décrire la
  # relation entre les variables dans votre modèle. Relisez les consignes
  # attentivement, si nécessaire.

  expect_true(is_identical_to_ref("fev_lmcomment"))
  # L'interprétation du modèle linéaire pour FEV et sa comparaison au modèle
  # linéaire génralisé est (partiellement) fausse
  # Vous devez cochez les phrases qui décrivent le modèle d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'smoke_glm', 'smoke_glmcomment', premier modèle linéaire généralisé pour Smoke", {
  expect_true(is_identical_to_ref("smoke_glm"))
  # Le premier modèle linéaire généralisé pour Smoke (chunk 'smoke_glm') n'est
  # pas le bon
  # Vérifiez en particulier la formule que vous avez écrite pour décrire la
  # relation entre les variables dans votre modèle. Relisez les consignes
  # attentivement, si nécessaire.

  expect_true(is_identical_to_ref("smoke_glmcomment"))
  # L'interprétation du premier modèle linéaire généralisé pour Smoke est
  # (partiellement) fausse
  # Vous devez cochez les phrases qui décrivent le modèle d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'smoke_glm2', 'smoke_compa', 'smoke_compacomment', second modèle linéaire généralisé pour Smoke et comparaison", {
  expect_true(is_identical_to_ref("smoke_glm2"))
  # Le second modèle linéaire généralisé pour Smoke (chunk 'smoke_glm') n'est
  # pas le bon
  # Vérifiez en particulier la formule que vous avez écrite pour décrire la
  # relation entre les variables dans votre modèle. Relisez les consignes
  # attentivement, si nécessaire.

  expect_true(is_identical_to_ref("smoke_compa"))
  # La comparaison des deux modèles généralisés pour Smoke (chunk 'smoke_compa')
  # n'est pas correcte
  # Vérifiez votre code et indiquez les modèles dans l'ordre comme arguments.
  # Vous devez aussi préciser que vous voulez un test "Chisq" ici.

  expect_true(is_identical_to_ref("smoke_compacomment"))
  # L'interprétation de la comparaison des deux modèles généralisés pour Smoke
  # est (partiellement) fausse
  # Vous devez cochez les phrases qui décrivent la comparaison d'un 'x' entre
  # les crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Chunks 'smoke_glm3', 'smoke_glm3comment', troisième modèle linéaire généralisé pour Smoke", {
  expect_true(is_identical_to_ref("smoke_glm3"))
  # Le troisième modèle linéaire généralisé pour Smoke (chunk 'smoke_glm3')
  # n'est pas le bon
  # Vérifiez en particulier la formule que vous avez écrite pour décrire la
  # relation entre les variables dans votre modèle. Relisez les consignes
  # attentivement, si nécessaire.

  expect_true(is_identical_to_ref("smoke_glm3comment"))
  # L'interprétation du troisième modèle linéaire généralisé pour Smoke est
  # (partiellement) fausse
  # Vous devez cochez les phrases qui décrivent le modèle d'un 'x' entre les
  # crochets [] -> [x]. Ensuite, vous devez recompiler la version HTML du
  # bloc-notes (bouton 'Rendu') sans erreur pour réactualiser les résultats.
  # Assurez-vous de bien comprendre ce qui est coché ou pas : vous n'aurez plus
  # cette aide plus tard dans le travail de groupe ou les interrogations !
})

test_that("Le code pour l'équation paramétrée du modèle Smoke est-il correct ?", {
  expect_true(rmd_select(lungcap, by_section(
    "Probabilité d'être fumeur")) |>
      as_document() |> is_display_param_equation("smoke_glm3"))
  # Le code pour générer l'équation paramétrée du modèle pour Smoke est
  # incorrect
  # Vous avez appris à faire cela dans un learnr du module 2. Revoyez cette
  # matière et vérifiez comment l'équation se présente dans le document final
  # généré avec le bouton ('Rendu').
})

test_that("La partie discussion et conclusion est-elle remplie ?", {
  expect_true(!(rmd_select(lungcap, by_section("Discussion et conclusion")) |>
      as_document() |> grepl("...Votre discussion à la place de ce texte...",
        x = _, fixed = TRUE) |> any()))
  # La discussion et la conclusion ne sont pas faites
  # Remplacez "...Votre discussion à la place de ce texte..." par vos phrases de
  # commentaires libres (à noter que le contenu de cette section n'est pas
  # évalué automatiquement, mais il le sera par vos enseignants).
})
