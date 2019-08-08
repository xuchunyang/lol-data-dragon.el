# Browse [Data Dragon](https://developer.riotgames.com/ddragon.html) in Emacs

![Image of champions and Teemo](Teeeemo.png)

## Requirement

- Emacs 25.1

## Setup

Download and decompress the ddragon file, then point `ddragon-dir` to it, for example,

``` shell
cd
wget https://ddragon.leagueoflegends.com/cdn/dragontail-9.15.1.tgz
tar xvzf dragontail-9.15.1.tgz
```

``` emacs-lisp
(setq ddragon-dir "~/dragontail-9.15.1/")
```

9.15.1 is the latest version as of today, Aug 7, 2019, you can find out the
latest version on https://ddragon.leagueoflegends.com/api/versions.json

## Usage

### `M-x ddragon-champion-image-dired`

Show all champions with `image-dired`, ie, a gallery of LoL champions.

### `M-x ddragon-champion-show-plain champion-id language-code`

Show information of a champion in plain text, including name, title, passive and
four spells.

## Resources

- https://developer.riotgames.com/ddragon.html (Data Dragon API)
- https://na.leagueoflegends.com/en/game-info/champions/ (en_US champions list)
- https://lol.qq.com/data/info-heros.shtml (zh_CN champions list)
- https://lol.garena.tw/game/champion (zh_TW champions list)
