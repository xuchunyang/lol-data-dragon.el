# Browse Champions of League of Legends on [Data Dragon](https://developer.riotgames.com/ddragon.html)

![Image of champions and Teemo](Teeeemo.png)

## Requirement

- Emacs 25.1

## Setup

Download and decompress the ddragon file, then point `lol-data-dragon-dir` to it, for example,

``` shell
cd
wget https://ddragon.leagueoflegends.com/cdn/dragontail-10.3.1.tgz
mkdir dragontail-10.3.1
tar xvzf dragontail-10.3.1.tgz -C dragontail-10.3.1
```

``` emacs-lisp
(setq lol-data-dragon-dir "~/dragontail-10.3.1/")
```

10.3.1 is the latest version as of today, Feb 13, 2020, you can find out the
latest version on https://ddragon.leagueoflegends.com/api/versions.json e.g.,

``` shell
~/s/lol-data-dragon.el $ curl -s https://ddragon.leagueoflegends.com/api/versions.json | jq -r .[0]
10.3.1
~/s/lol-data-dragon.el $
```

## Usage

### `M-x lol-data-dragon-champion-image-dired`

Show all champions using `image-dired`.

### `M-x lol-data-dragon-champion-show-QWER champion-id language-code`

Display abilities (passive and QWER) of a champion.

### `M-x lol-data-dragon-champion-show-skins champion-id`

Display all skins of a champion.

### `M-x lol-data-dragon-champion-show-tiles champion-id`

Display all tiles of a champion.

### `M-x lol-data-dragon-random-random-tiles`

Display N random tiles.

### `M-x lol-data-dragon-list-champions-in-org-table language-code`

List all champions in Org mode table.

## API

### `(lol-data-dragon-champions)`

Return a list of champions IDs.

``` emacs-lisp
(lol-data-dragon-champions)
;; => ("Aatrox" "Ahri" "Akali" "Alistar" "Amumu" "Anivia" "Annie" "Aphelios" "Ashe" ...)
```

### `(lol-data-dragon-champions-data LANG)`

Return all champions' data in LANG as a list.

``` emacs-lisp
(let-alist (alist-get 'Teemo (lol-data-dragon-champions-data "zh_CN"))
  .name)
;; => "迅捷斥候"
```

## Resources

- https://developer.riotgames.com/ddragon.html (Data Dragon API)
- https://na.leagueoflegends.com/en/game-info/champions/ (en_US champions list)
- https://lol.qq.com/data/info-heros.shtml (zh_CN champions list)
- https://lol.garena.tw/game/champion (zh_TW champions list)
