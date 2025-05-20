;;; wechat-emoji.el ---                              -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar wechat-emojis-image-file
  (file-name-concat
   (file-name-directory (or load-file-name (buffer-file-name)))
   "resources"
   "wechat-emojis.png"))

(defvar wechat-emojis
  '("[微笑]" "[撇嘴]" "[色]" "[发呆]" "[得意]" "[流泪]" "[害羞]" "[闭嘴]" "[睡]" "[大哭]"
    "[尴尬]" "[发怒]" "[调皮]" "[呲牙]" "[惊讶]" "[难过]" "[囧]" "[抓狂]" "[吐]" "[偷笑]"
    "[愉快]" "[白眼]" "[傲慢]" "[困]" "[惊恐]" "[憨笑]" "[悠闲]" "[咒骂]" "[疑问]" "[嘘]"
    "[晕]" "[衰]" "[骷髅]" "[敲打]" "[再见]" "[擦汗]" "[抠鼻]" "[鼓掌]" "[坏笑]" "[右哼哼]"
    "[鄙视]" "[委屈]" "[快哭了]" "[阴险]" "[亲亲]" "[可怜]" "[笑脸]" "[生病]" "[脸红]" "[破涕为笑]"
    "[恐惧]" "[失望]" "[无语]" "[嘿哈]" "[捂脸]" "[奸笑]" "[机智]" "[皱眉]" "[耶]" "[吃瓜]"
    "[加油]" "[汗]" "[天啊]" "[Emm]" "[社会社会]" "[旺柴]" "[好的]" "[打脸]" "[哇]" "[翻白眼]"
    "[666]" "[让我看看]" "[叹气]" "[苦涩]" "[裂开]" "[嘴唇]" "[爱心]" "[心碎]" "[拥抱]" "[强]"
    "[弱]" "[握手]" "[胜利]" "[抱拳]" "[勾引]" "[拳头]" "[OK]" "[合十]" "[啤酒]" "[咖啡]"
    "[蛋糕]" "[玫瑰]" "[凋谢]" "[菜刀]" "[炸弹]" "[便便]" "[月亮]" "[太阳]" "[庆祝]" "[礼物]"
    "[红包]" "[福]" "[烟花]" "[猪头]" "[跳跳]" "[发抖]" "[转圈]"))

(defvar wechat-emojis-en
  '("[Smile]" "[Grimace]" "[Drool]" "[Scowl]" "[CoolGuy]" "[Sob]" "[Shy]" "[Silent]" "[Sleep]" "[Cry]" "[Awkward]" "[Angry]" "[Tongue]" "[Grin]" "[Surprise]" "[Frown]" "[Blush]" "[Scream]" "[Puke]" "[Chuckle]" "[Joyful]" "[Slight]" "[Smug]" "[Drowsy]" "[Panic]" "[Laugh]" "[Commando]" "[Scold]" "[Shocked]" "[Shhh]" "[Dizzy]" "[Toasted]" "[Skull]" "[Hammer]" "[Bye]" "[Speechless]" "[NosePick]" "[Clap]" "[Trick]" "[Bah！R]" "[Pooh-pooh]" "[Shrunken]" "[TearingUp]" "[Sly]" "[Kiss]" "[Whimper]" "[Happy]" "[Sick]" "[Flushed]" "[Lol]" "[Terror]" "[Let Down]" "[Duh]" "[Hey]" "[Facepalm]" "[Smirk]" "[Smart]" "[Concerned]" "[Yeah!]" "[Onlooker]" "[GoForIt]" "[Sweats]" "[OMG]" "[Emm]" "[Respect]" "[Doge]" "[NoProb]" "[MyBad]" "[Wow]" "[Boring]" "[Awesome]" "[LetMeSee]" "[Sigh]" "[Hurt]" "[Broken]" "[Lips]" "[Heart]" "[BrokenHeart]" "[Hug]" "[ThumbsUp]" "[ThumbsDown]" "[Shake]" "[Peace]" "[Salute]" "[Beckon]" "[Fist]" "[OK]" "[Worship]" "[Beer]" "[Coffee]" "[Cake]" "[Rose]" "[Wilt]" "[Cleaver]" "[Bomb]" "[Poop]" "[Moon]" "[Sun]" "[Party]" "[Gift]" "[Packet]" "[Blessing]" "[Fireworks]"  "[Pig]" "[Waddle]" "[Tremble]" "[Twirl]"))

(defvar wechat-emoji-locations-map nil)

(defun wechat-emoji--locations-map ()
  (let ((table (make-hash-table :test #'equal))
        i j)

    (cl-loop for idx from 0 to (length wechat-emojis)
             do
             (setq i (/ idx  10))
             (setq j (% idx 10))
             (puthash (nth idx wechat-emojis-en) (cons i j) table)
             (puthash (nth idx wechat-emojis) (cons i j) table))
    table))

(defun wechat-emoji-format-image (name)
  (unless wechat-emoji-locations-map
    (setq wechat-emoji-locations-map (wechat-emoji--locations-map)))
  (let ((str name))
    (when-let* ((location (gethash name wechat-emoji-locations-map))
                (image (create-image wechat-emojis-image-file
                                     nil nil
                                     :width (* 10 (line-pixel-height))
                                     :height (* 11 (line-pixel-height))))
                (width (line-pixel-height))
                (height (line-pixel-height))
                (x (* width (cdr location)))
                (y (* height (car location))))
      (setq str
            (propertize
             "."
             'display
             (list (list 'slice x y width height) image))))
    str))


(defun wechat-emoji-annotation-function (candidate)
  "Return annotation for CANDIDATE."
  (let ((annotation (wechat-emoji-format-image candidate)))
    (when annotation
      (concat "  " annotation))))

(defun wechat-emoji-input ()
  "Select an emoji to input."
  (interactive)
  (let ((completion-extra-properties
         '(:annotation-function wechat-emoji-annotation-function)))
    (insert (completing-read "Select an emoji: "
                             wechat-emojis))))

(provide 'wechat-emoji)
;;; wechat-emoji.el ends here
