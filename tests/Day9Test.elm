module Day9Test exposing (..)

import Day9
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "day 9"
        [ test "part 1 example" <|
            \() ->
                exampleInput
                    |> Day9.parseInput
                    |> Maybe.andThen Day9.solvePart1
                    |> Expect.equal (Just 15)
        , test "part 1 answer" <|
            \() ->
                puzzleInput
                    |> Day9.parseInput
                    |> Maybe.andThen Day9.solvePart1
                    |> Expect.equal (Just 436)

        --, test "part 2 example" <|
        --    \() ->
        --        exampleInput
        --            |> Day9.parseInput
        --            |> Maybe.andThen Day9.solvePart2
        --            |> Expect.equal (Just 61229)
        --, test "part 2 answer" <|
        --    \() ->
        --        puzzleInput
        --            |> Day9.parseInput
        --            |> Maybe.andThen Day9.solvePart2
        --            |> Expect.equal (Just 979171)
        ]


exampleInput : String
exampleInput =
    """
2199943210
3987894921
9856789892
8767896789
9899965678
"""


puzzleInput : String
puzzleInput =
    """
6769876887698999876367898543212378997654321291098765432398767667989976543210123456987678999766598921
5456965476567999985457897654301456989743210989989898540987656545678987654323634679876569998757387892
4349864345476889876778999864323699879654329879878987659876543434569998789434545798765459879543296989
3298753212345678989889298765459989768965498764567898899865432523459999998587657987654365965654135678
4349432101346789999992169876798778957896796553456789956976921012367899987699788998543234984321015789
5498763212457899999743456989987667945997985432377894344989854323456789998989999987642149765632134899
6569854343568999897654599797898543234589876545688943243498765434678999999878891298753259898543236789
7679878987699598789775988676987632123578987656799654102349876545678999898566789987654345997654545678
8999989398789699678989876545698821014567898787898743212357987656789998787355679999965456789765656799
9889895459893987565699876123799935223698979898987654523456898967899987676234589999876767899878767898
8679789599912398434989765234899876434899765989998765634567989978969876535123467892987878944989878987
7567678989101995429878984349998989545999854677899876749879878989656998321012678921298989432192989656
5434569878919876998767895498987987656789543456789987857998767898945987633234567899349999543943496545
6323498767899989897656789987676698767898632355678998998987654567932398545345678988999987659894965436
3210997856789998756545679878534569898987321234567899799999543656891239659459789467989998798769876547
4329896545456987643334789964323457989876320125688965679898932345994398798998994359878999987756987678
5679765432346987532123458953212345678985431234599954398767921256789459987987893249867899876545698789
6789874321234597645634567894101558789996563545999895987656890127899569876546789197757598765434569896
9892983210145698656747698943212467893987854559886789998967892234568978965435678986543459876545678945
4921294929236798787958999656543569912398967699775698999878964345878999974324567996432345987676899432
3210239898947899898767898769654578923999878987654567896989875458989999865435678987320123498787976561
9854398787898999999898999898765689439899989299985898995490986567899989978947799987431434569898997610
8769987676799998789969899949976896598789890199876789989321299678978978599656789876532345678979989891
9898787545598987683456789534987897989678789987987895878932398789569765498767894987673476789764678989
9987655633467896542347892124998949876565678976598934567893499893488964349878923498784587890123699875
9996543212356789451234589039899934965454567895439123456954987942567899219999436569895698921234589964
8987654101236893210123478949767899876323458987321012345699876543478988998999987678976789434345678943
7898987654345789345234569998656787998434767896542324556789987654789876787898799789987896545756789432
6789698895676997656745699887545476899547898987955456789895698765698765456789678999899998787867894321
5456569989987898767896789786431265987656999099876689892934569876987762347896569878768989898978943210
4343459878998939898987897654320123498767892129998994921012489989876543456789398765457679999999976731
3232349767899323989998999865434634569979943398999323932125678997989654567893219854324587899989895432
2101298956789219878999999979548795997898954997678919865434589876598785678954109543213456789878789543
3432397545694398767899989987656989886887899876567899876745789985419876789865998432101578898769698956
4563498434989459856989879998769878765976789965456789989899899954323987999999876563234567987654567897
5654984323878998768978568999898965654365678974345678991998987895439999548789997696549699876543476998
6979876412767899979765457989987654321254569893234789890987896989598965434598998989698989985432345699
9899865401256789989987679878698765410123456789123456789896645878987654324687899878987679876321234589
9798763212346894394399899859569896521434567893256599898795434767999743212356898767496589986410124679
8679954323567989239212999743456985432545778965346789987654323456789894101234987656325498765421235678
6569865434689678998909998632109876543656889876657899876543213456898765213456976543212349975434547899
5450976545696567997898976543238989656767996987768976998654354567897654324587987643201467986545698945
7321987968789459876567897654347898767898965498979895349765455678998765437678998754312588997656789323
5432398979899598765498989765956999979939654329296789239878567889549876548789898765423678999869893219
6543459989998679876989878979897898989529873210145892145989678999932987679896789876739789896978954998
7656569998999789989876967898799967895434998721234799259998789878893498989945678989849898765989969897
8767678967899994598765458945679656976565987632465678998999896566789599799434567899956987654197898776
9898989656789543459884301236789745697878998543456789987899975465698987678923459989878998543236789545
3999898745897692196543212345678956789989987657578899896789764314567898567894569878989999664545678934
1298769896789989987654523898789879891099898968689998765699995423458965498789679768999989775656799224
0989899999899878999765674789899998932198769989789349954569876534567894349678998957899879876767891012
9878989998998769899876785678998797893239654399895456967678987687678943254567987846789954987898932199
9767679987865456789989876789989686789398763212976769878789798798789652123979876535678893298999943988
7754579876764345678999989899876545678909854323989878989897679899898761019899987621236789129498769877
6543569885323236567897899999998656789212965439992999999978568999987653198789987540345891012389879656
7632398764210123489965458998798767896369879598901298998769456789999964987667895431456789325478998945
5421449875521238567896367987659988965456998987892987569954345678999899876543976432347895434567987934
6530123985434347678963219876545699896567997676789645459893234569989789987659876545456976765679876321
6521234596545458789954398987632346789679876585878932398789345698875678998789987678967897876789985410
8434345987657679899899987654321456898789965434567891987695467987764567899897698789598998998993494321
7545656798788789935678998985490123499999987323458989876569568996543456921976549896459789219654976452
7658769899899892123789109876989234989898765434568969875458979987632345890987698965345678929769876543
8769878987956921094599212989878949876769876745878957987567894596543456791998987654234567899878989655
9878989896545699989698993498769998765456988656789345699678943987654567899899698754345678923989898767
0989998787435678978987889987758789876323499767891234798789432398789878998765539965476989919899789878
1296987658324234569896569876646678987212349898910147899896541019899989799654429876567896798788678989
2345698943210123698765456985434568998101236989321256789987893223978998688943212989778945987666567899
3459987654321235987654329876523457899212345678932347899998987654567897567892101497989239876555456789
4598998796532349898543512987212678954393456789765478999899998785678943459983212345990198765436345999
9987889987643498765432109832103589976989569898997567898788999899889012599876364587891239876521234789
8956776798754569876543498765412367899878998946789678987657899910994123989765456698992946983210345678
7842365679897678987674987654323456789967987897896989498746989329873249878976767899989897894323458989
6531234589998989199876799985476567896754976789945799395439978998765398767897878901978789987434567899
7810123478999899012987899876587678975463465678935678989598768989887469898998989329765679876545678998
8921335567897778943498987987998789764322234569024599878987659879998567979999699498974598987656789347
9432456678976567894999976598939897653210123478934988767998545568989878967894578987653987998968991236
6543467789865478999899865439123998965323234569549878542987632459976989457993567987542196569879210145
7656569898976567898767974321045679879434545678998765431298756567895490346989879898943987899989323234
9867878967987689989856985434756789989565676789459976530459767878989321259978998789764598998996545345
9878989456798789876549876545969891297678797892345987321346998989878932498767897678975989987987675456
9989992345699899995432989658998992998789898901239876434456789596567893998654343487899879896598786568
9898943458789989989321298767987989869896969912345987545589895433456789876543212396789868789439897678
8797899879899879878934569879896878954945457893957897656678985322457899987752101245678947678921998789
7656789989998664767895699999765767893234356789898998767989876301367999898943212386789234567892369893
7545691099876543456789989987654456789101299895789459989999989212479998789654563498992123679965456921
5434593198767552325699878999732345898999989954678969899998765323589987698765678989893234567896567890
0125989987654321014598765987621234567988978912389998799879876494999896539898789678794347678987699921
3234978998798775123987654599434348979876767923498999689765989989898797921999894545689956989998989932
4549767899899654234599743398765457899765456895987698578954598875654689892988953236568897899989877893
5998456789999874345987652109979567987854345679998567467895987654343456789877542123456798999875766789
9876587893298765659876543212398999996543234567895432358999897543232345698765431012345689898754345899
4997678954109878767987676378987678987642123458789521235798765432101256789876542123456998799343234789
3498789543299989878999785459876567995431012345678944345699896953632367892987853234569877679210145678
6569899654989597989239876569985479876532123456899765656789979876543456943498964348798963458921234567
9699998799878456899123998698794321987843234569999876767898767987854567894579875499987654567894345789
8989219987656345678934599987653210198967347678989987898987654398765679965679876989999965878965756899
7478909876544234567895678998764321239878456789679998969898321239989989876989989878989876789879867998
6367899998432123456789899219965432346989767896568999656789432387895491987893498767678989894989878987
5256789987641015667899964329876563456799898965457898547896543456954320198912987654567899953492989876
4345896595432124588998765545987674567899999876345987658987659767896431239109876543456789432101299965
"""
