	include	"custom.i"
		
;--------
;- INIT -
;--------

	move.l	4,a6		;execbase
	jsr	-132(a6)	;forbid()

;alloc mem

	move.l	#$3200,d0	;reserve memoire pour bitplan 400*256
	move.l	#$10002,d1	;en chipmem
	jsr	-198(a6)
	move.l	d0,bitplan	;adresse memoire reservee
	beq	end		
	move.w	d0,pth+6	;update de la copperlist
	swap	d0		;en mettant a jour les pointeurs
	move.w	d0,pth+2	;de plan a l'adresse reservee

;pointeurs sprite	
	move.l	#heart,d0	;update des pointeurs sprite
	move.w	d0,spr+6	;de la copperlist
	swap	d0
	move.w	d0,spr+2

;save old copperlist

	lea	gfxname,a1
	clr.l	d0
	jsr	-552(a6)	;open graphics.library
	beq	end
	move.l	d0,a1
	move.l	38(a1),oldcop	;sauvegarde adresse old copperlist
	jsr	-414(a6)	;close graphics.library

;init new DMA, int, and clist

	lea	custom,a0
	move.w	dmaconr(a0),d0
	ori.w	#$8000,d0		;DMA SET/CLR = 1
	move.w	d0,olddma		;sauvegarde ancienne DMA
	bsr	waitvbl			;pour eviter bug sprite
	move.w	#$7FFF,dmacon(a0)	;stop toutes DMA
	lea	clist,a2
	move.l	a2,cop1lc(a0)		;adresse nouvelle copperlist
	clr.w	copjmp1(a0)		;reset copper
	move.w	intenar(a0),d0
	ori.w	#$8000,d0		;IRQ SET/CLR = 1
	move.w	d0,oldintena		;sauvegarde ancienne INT
	move.w	#$7FFF,intena(a0)	;stop toutes IRQ
	move.l	$6c.w,oldinter		;sauvegarde ancien vecteur level 3 du 68K
	move.l	#vblint,$6c.w		;mise en place nouveau vecteur
	move.w	#$C020,intena(a0)	;start VBL int
	move.w  #$83E0,dmacon(a0)	;start Raster+Copper+Blitter+Sprite DMA
	lea	mt_data,a0
	bsr	mt_init			;init protracker replay routine
	
;-------------------------------------------
;- Intro code : just calls to sub-routines -
;-------------------------------------------
;init de quelques valeurs
textbase = 50*50+4
scrollbase = $2C7E
	move.w	#9,sprposcounter	;10 positions differentes pour le sprite
	move.w	#4,frames		;frameskip pour deplacement sprite
	lea	scrolltext,a2
	move.l	a2,scrolltextptr	;init pointeur sur scrolltext

;recopie des logos
	move.l	bitplan,a0
	add.l	#13,a0			;centrage logo
	lea	logo,a2
	move.l	#15,largeur
	move.l	#39,hauteur
	bsr	cbmap
	move.l	bitplan,a0
	add.l	#6307,a0		;ajustement positionnement
	lea	amilogo,a2
	move.l	#23,largeur
	move.l	#42,hauteur
	bsr	cbmap

;routine intro
intro	move.l	bitplan,a0
	add.l	#textbase,a0
	lea	text,a2
	bsr	wtxt
	btst	#6,$bfe001
	beq	end
	bra	intro
	
;----------------
;- Display text -
;----------------
	
;ecrit un bloc de caracteres, a0 = destination dans le bitplan, a2 = texte a recopier
wtxt	move.l	a0,a3		;sauvegarde de la position de debut de ligne
debl	clr.l	d0	
	move.b	(a2)+,d0	;lit prochain caractere a afficher
	beq	finl		;zero ? -> fin ligne
	cmp.b	#$FE,d0		;FE? -> fin paragraphe
	beq	finp
	cmp.b	#$FF,d0		;FF? -> fin texte
	beq	fint
	bsr	wchar
	adda.l	#1,a0
	bra	debl
finl	adda.l	#9*50,a3	;fin de ligne -> on positionne le pointeur a la ligne suivante
	move.l	a3,a0
	bra	debl		;et on recommence
finp	bsr	wait
	bsr	cls
	move.l	bitplan,a0
	add.l	#textbase,a0
	bra	wtxt
fint	bsr	wait
	bsr	cls
	rts
		
;ecrit le caractere donne dans d0 a l'adresse donnee dans a0
wchar	subi.b	#$20,d0	
	lsl.l	#3,d0		;offset bitmap caractere
	lea	font,a1		;on additionne cet offset a l'adresse de base de la font
	adda.l	d0,a1		;pour obtenir l'adresse du caractere
	move.b	(a1)+,(a0)	;recopie 1 ligne de 8 octets = 8 pixels
	move.b	(a1)+,50(a0)
	move.b	(a1)+,100(a0)
	move.b	(a1)+,150(a0)
	move.b	(a1)+,200(a0)
	move.b	(a1)+,250(a0)
	move.b	(a1)+,300(a0)
	move.b	(a1)+,350(a0)
	rts			;termine
	
;--------------------------------------
;- Copy a 1 bitplane bitmap to screen -	
;--------------------------------------

cbmap	move.l	hauteur,d2		;hauteur en lignes
l1	move.l	largeur,d1		;largeur en octets
	move.l	a0,a3			;sauvegarde debut ligne
l2	move.b	(a2)+,(a0)+
	dbf	d1,l2			;recopie 1 ligne
	subi	#1,d2
	beq	l3
	add.l	#50,a3			;pointeur sur ligne suivante
	move.l	a3,a0
	bra	l1
l3	rts

;-------------------------------
;- Clear textarea with Blitter -
;-------------------------------

cls	lea	custom,a0
	move.l	bitplan,a1
	add.l	#textbase,a1
	bsr	waitblit
	move.l	#$01000000,bltcon0(a0)
	move.l	#$ffffffff,bltafwm(a0)
	move.w	#0,bltdmod(a0)
	move.l	a1,bltdpt(a0)
	move.w	#(40<<6)+25,bltsize(a0)
	bsr	waitblit
	rts

waitblit
	lea	custom,a6
	btst.b 	#6,dmaconr(a6)
	btst.b 	#6,dmaconr(a6)
	bne	waitblit
	rts

;---------------------------
;- Level 3 interrupt (VBL) -
;---------------------------

vblint	movem.l	d0-d7/a0-a6,-(a7)	;sauve registres sur pile
	tst.w	counter			;on teste le compteur
	bne	scroll			;si pas zero:on doit encore scroller
	move.w	#8,counter
	move.l	scrolltextptr,a2
scroll1	move.l	bitplan,a0		;sinon on reinitialise le pointeur
	add.l	#scrollbase,a0		;pour recopie prochain caractere
	clr.l	d0
	move.b	(a2)+,d0
	move.l	a2,scrolltextptr	;sauvegarde pointeur de texte
	
	tst.b	d0			;fin texte ?
	beq	scroll2
	bsr	wchar			;non->on ecrit le caractere,on scrolle, on decremente le compteur

scroll	lea	custom,a6		;on scrolle d'un pixel vers la gauche
	move.l	bitplan,a0
	add.l	#$2DE4,a0
	bsr	waitblit
	move.l	#$19F00002,bltcon0(a6)	;btlcon0 + bltcon1 ASH1, USEA, USED = 1
	move.l	#$ffffffff,bltafwm(a6)	;bltafwm + bltalwm
	move.l	#0,bltamod(a6)		;bltamod + bltdmod
	move.l	a0,bltapt(a6)
	move.l	a0,bltdpt(a6)
	move.w	#(8<<6)+25,bltsize(a6)
	subq.w	#1,counter


	tst.w	frames
	bne	dmove
	move.w	#4,frames		;frameskip pour deplacement sprite = 4
	clr.l	d0
	move.w	sprposcounter,d0	;compteur des positions successives
	cmp.w	#9,d0			;9 positions au total
	bne	pos1		
	clr.w	d0			;9e position? on reinitialise
	move.w	d0,sprposcounter	
pos1	lsl.w	#2,d0			;multiplie compteur par 2 pour
	lea	pos,a1			;l'offset qui pointe sur les
	lea	heart,a2		;prochaines donnees
	move.l	(a1,d0),(a2)
	add.w	#1,sprposcounter	;incremente compteur des positions
dmove	subq.w	#1,frames

	bsr	mt_music		;routine replay module

	move.w	#$4020,intreq(a6)	;efface flag d'interrupt
	movem.l	(a7)+,d0-d7/a0-a6 	;recupere registres de la pile
	rte	

scroll2	lea	scrolltext,a2		;fin texte=on reinitialise le
	move.l	a2,scrolltextptr	;pointeur au debut du texte
	bra	scroll1
	
;---------------
;- Test mouse -
;---------------

mouse	btst	#6,$bfe001
	bne	mouse
	rts
	
;------------
;- Wait VBL -
;------------

waitvbl	cmp.b	#255,$DFF006	;vhposr (beam position counter)
	bne.s	waitvbl
	rts

;------------
;- Wait 10s -
;------------

wait	clr.b	todmid
wloop	btst	#6,$bfe001
	beq	endwait
	cmp.b	#$02,todmid
	bne	wloop
endwait	rts

;------------------------------------
;- Restore old DMA, IRQ, copperlist -
;------------------------------------
	
end:	bsr	mt_end			;fin musique
	lea	custom,a0	
	move.w	olddma,dmacon(a0)	;restore de l'ancienne DMA
	move.w	oldintena,intena(a0)	;restore anciennes INT
	move.l	oldinter,$6c.w		;restore ancien vecteur
	move.l	oldcop,cop1lc(a0)	;restore la copper list systeme
	move.l	bitplan,a1
	move.l	#$3200,d0
	move.l	4,a6		;execbase
	jsr	-210(a6)		;freemem
	jsr	-138(a6)		;permit
	clr.l	d0			;code retour
	rts
	
;----------------------
;- Some defs and incs -
;----------------------

frames	dc.w	0	
counter	dc.w	0
sprposcounter	dc.w	0
bitplan	dc.l	0
scrolltextptr	dc.l	0
oldcop	dc.l	0
olddma	dc.w	0
oldintena	dc.w	0
oldinter	dc.l	0
largeur		dc.l	0
hauteur		dc.l	0
gfxname	dc.b	"graphics.library",0
font	incbin	"sources:2nd-intro/assets/font2_8x8.bin"
logo	inciff	"sources:2nd-intro/assets/kaiser.iff"
amilogo	inciff	"sources:2nd-intro/assets/amiga_small.iff"
	even
	include "ptreplay.s"
	
;--------------------
;- Sprite positions -
;--------------------

	even
pos	dc.w	$9292,$9C00
	dc.w	$9392,$9D00
	dc.w	$9592,$9F00
	dc.w	$9992,$A300
	dc.w	$A192,$AB00
	dc.w	$9992,$A300
	dc.w	$9592,$9F00
	dc.w	$9392,$9D00
	dc.w	$9292,$9C00
	
;-------------
;- Text data -
;-------------

text	dc.b	"              Kaiser",0
	dc.b	"           presents you",0
	dc.b	"         his second intro",$FE
	dc.b	"  Once again, I had a lot of fun",0
	dc.b	"  while coding these few bytes !",0
	dc.b	"  and learnt so many new things.",$FE
	dc.b	"  I'd like to send hellos to all",0
	dc.b	"     those who keep the Amiga",$0
	dc.b	"  spirit alive! Special thanks:",0
	dc.b	"  Pmc & Zeroblue for their help.",$FE
	dc.b	"    If you want to contact me,",0
	dc.b	"  write to nbauw(at)hotmail.com",0
	dc.b	"    I'd love to form or join a",0
	dc.b	"   group for intros or demos !!",$FE
	dc.b	"    Don't forget to visit the",0
	dc.b	"   Amiga Demoscene Archive site",0
	dc.b	"       ada.untergrund.net !",$FE
	dc.b	"  Credits for this intro : code,",0
	dc.b	"  design and ugly gfx by Kaiser.",0
	dc.b	"  --> Great tune by Nainain! <--",0
	dc.b	"    Text font found on the net.",$FF
	
scrolltext	dc.b	"Hi this is Kaiser on the keyboard "
		dc.b	"talking on his first scrolltext !"
		dc.b	" Scrolltexts may seem outdated, but I'm "
		dc.b	"a huge fan of oldschool stuff, especially "
		dc.b	"demos from the 1991-1992 era. I remember "
		dc.b	"I loved reading scrolltexts in Phenomena "
		dc.b	"Enigma and Silents Global Trash. In my "
		dc.b	"opinion, scrolltexts are like a picture "
		dc.b	"of people who made a demo at a given time "
		dc.b	"of their life, it tells a lot on what "
		dc.b	"the demoscene was at that time ! Nostalgia time "
		dc.b	"for me ! This small learning-intro is my second one, still using a 1-bitplane screen, and was finished in January 2012. "
		dc.b	"Since my first intro (originally written in 2005, debugged and finalized a few weeks ago), I learnt quite a lot : "
		dc.b	"how to use the timer, interrupts, sprites, and the blitter to clear screen and make... "
		dc.b	"yeah you got it, scrolltexts :-) Some people "
		dc.b	"talked me about releasing an intro for "
		dc.b	"Revision 2012... I'd love to but I think "
		dc.b	"my coding skills won't be impressive enough ! "
		dc.b	"Still, I'm very happy to keep on learning "
		dc.b	"and coding. See you in my next intro...                        "
		dc.b	"end of line...                                           ",0

;--------------
;- Copperlist -
;--------------

	section	copper,data_c
	
clist	dc.w	bplcon0,$1000	;1 bitplan lores non interlace
	dc.w	bplcon1,$0000 
	dc.w	ddfstrt,$0038
	dc.w	ddfstop,$00D0
	dc.w	diwstrt,$2C81
	dc.w	diwstop,$2CC1
	dc.w	bpl1mod,$000A
	dc.w	$01A2,$0FA8		;couleur sprite 0 : rose
	dc.w	$01A4,$0F00		;couleur sprite 0 : rouge
	dc.w	$0180,$0004
	dc.w	$0182,$0FFF
pth	dc.w	$00E0,0,$00E2,0	;BPL1PTH et BPL1PTL
spr	dc.w	$0120,0,$0122,0	;SPR0PTH et SPR0PTL
	dc.w	$0124,$0000,$0126,$0000
	dc.w	$0128,$0000,$012a,$0000
	dc.w	$012c,$0000,$012e,$0000
	dc.w	$0130,$0000,$0132,$0000
	dc.w	$0134,$0000,$0136,$0000
	dc.w	$0138,$0000,$013a,$0000
	dc.w	$013c,$0000,$013e,$0000
	dc.w	$2801,$FF00,$0182,$0EEE
	dc.w	$3C01,$FF00,$0182,$0DDD
	dc.w	$4001,$FF00,$0182,$0CCC
	dc.w	$4501,$FF00,$0182,$0AAA
	dc.w	$4801,$FF00,$0182,$0999
	dc.w	$5001,$FF00,$0180,$0444
	dc.w	$5041,$FFFE,$0180,$0555
	dc.w	$5051,$FFFE,$0180,$0666
	dc.w	$5061,$FFFE,$0180,$0777
	dc.w	$5071,$FFFE,$0180,$0888
	dc.w	$5081,$FFFE,$0180,$0999
	dc.w	$5091,$FFFE,$0180,$0AAA
	dc.w	$50A1,$FFFE,$0180,$0BBB
	dc.w	$50B1,$FFFE,$0180,$0CCC
	dc.w	$50C1,$FFFE,$0180,$0DDD
	dc.w	$50D1,$FFFE,$0180,$0EEE
	dc.w	$50E1,$FFFE,$0180,$0FFF
	dc.w	$5101,$FF00,$0180,$0004,$0182,$0DDD

	dc.w	$A001,$FF00,$0182,$048E
	dc.w	$A501,$FF00,$0182,$037A
	dc.w	$AA01,$FF00,$0182,$0369
	dc.w	$AF01,$FF00,$0182,$0259
	dc.w	$B401,$FF00,$0182,$0248
	dc.w	$B901,$FF00,$0182,$0137
	dc.w	$BE01,$FF00,$0182,$0136
	dc.w	$C301,$FF00,$0182,$0026
	dc.w	$C801,$FF00,$0182,$0025
	dc.w	$CD01,$FF00,$0182,$0006
	dc.w	$D601,$FF00,$0182,$0DDD

	dc.w	$FD01,$FF00,$0180,$0FFF
	dc.w	$FD41,$FFFE,$0180,$0EEE
	dc.w	$FD51,$FFFE,$0180,$0DDD
	dc.w	$FD61,$FFFE,$0180,$0CCC
	dc.w	$FD71,$FFFE,$0180,$0BBB
	dc.w	$FD81,$FFFE,$0180,$0AAA
	dc.w	$FD91,$FFFE,$0180,$0999
	dc.w	$FDA1,$FFFE,$0180,$0888
	dc.w	$FDB1,$FFFE,$0180,$0777
	dc.w	$FDC1,$FFFE,$0180,$0666
	dc.w	$FDD1,$FFFE,$0180,$0555
	dc.w	$FDE1,$FFFE,$0180,$0444
	dc.w	$FE01,$FF00,$0180,$0004
	dc.w	$FFFF,$FFFE	;fin copperlist

;---------------
;- Sprite data -
;---------------
	
	even
heart	dc.w	0,0
	dc.w	%001110000011100,%000000000000000
	dc.w	%010001000100010,%001110000011100
	dc.w	%100000101000001,%011111000111110
	dc.w	%010000010000010,%001111101111100
	dc.w	%001000000000100,%000111111111000
	dc.w	%000100000001000,%000011111110000
	dc.w	%000010000010000,%000001111100000
	dc.w	%000001000100000,%000000111000000
	dc.w	%000000101000000,%000000010000000
	dc.w	%000000010000000,%000000000000000
	dc.w	$0000,$0000

;---------------
;- Module data -
;---------------

mt_data	incbin	"asmpro:sources/2nd_intro/mod.neverdie16"
