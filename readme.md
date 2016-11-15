<h1> Udkast til arbejdsplan </h1>
Til gruppens brug.

<h3> Rapport </h3>

Se .tex fil:

Dokumentér kode løbende - og lad os sætte os i ml. 14.11.16-17.11.16 og koordinere. 


<h3> Funktioner </h3>

Vi skal konstruere følgende funktioner til spillet Mastermind:

<ul style="list-style-type:circle">
	<li><h5> Validate
		<li><h5> Hertil blackbox-testing af funktionalitet </h5></li></h5></li>
	<li><h5> makeCode </h5></li>
	<li><h5> guess </h5></li>

<h4> Validate </h4>
Skriver pseudokode: 

Lad guess = {Red, Blue, Black, White}

Lad code = {Red, Black, Blue, White}

```shell
Validate(guess, code) =
	if guess == code --> {Black, Black, Black, Black} --> "Win!"
	else do
		x = count(guess.contains(code))
		if x > 0 do
			for i = 0 to code.Length do
				z = count.if.TRUEcode[i] == guess[i])
		x = x - z
	printfn "Black : %A, White : %A" z x
```


Mhp. BB-testing: Kunne man afprøve validate mod 2*random(makeCode) over x iterationer?


<h4> makeCode </h4>

Se .fsx

<h4> guess </h4>

Ikke afgjort endnu

<h3> Mødetidspunkter </h3>

13.11.2016 @ 10:00  -  Hos Peter/Adam
15.11.2016 @ 10:00  -  Hos Peter/Adam

Lad os tage bestik af situationenen herfra. 

<h3> Arbejdsfordeling </h3>

Endnu ikke fastlagt. 