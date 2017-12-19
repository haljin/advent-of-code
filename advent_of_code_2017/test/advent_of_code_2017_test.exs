defmodule AdventOfCode2017Test do
  require AdventOfCode2017.AdventMacros
  import AdventOfCode2017.AdventMacros
  use ExUnit.Case

  adventdoctest()

  test "Real solutions for day 1" do
    data = "649713959682898259577777982349515784822684939966191359164369933435366431847754488661965363557985166219358714739318371382388296151195361571216131925158492441461844687324923315381358331571577613789649" <>
    "16648615223794591798797779389173986514973475599324136188633692653848227112475535957279145133584253489319269355865999117198384928548913942142593363861488441589693891499273249219245863648452322824453233158758" <>
    "47795527885446672535773246499152741159246117583456761834439829927339663734983856859657689292414779837279212798267279768725563154284347991617597349326598299345623393853281196568234839548564273658926277281635" <>
    "24721467938449943358192632262354854593635831559352247443975945144163183563723562891357859367964126289445982135523535923113589316417623483631637569291941782992213889513714525342468563349385271884221685549996" <>
    "53433376573124389566262482992498297168544382536682792358943525451421148964948237487643454968278545969888552167325893941325515819652569623645791144759994744966554255425148684738882357693716723747655678213322" <>
    "72793245268349465344447181615241292859194779599376847288825927799417341861441388839943227424848539253835186516871472469434213112873248676636984325466195836389766377333452518348699857463853716177434986271114" <>
    "41933546356934671639545342515392536574744795732243617113574641284231928489312683617154536648219244996491745718658151648246791826466973654765284263928884137863647623237345882469142933142637583644258427416972" <>
    "59524173725444971853172417653864836925379668893124519138295696154477585687228131774382855262984355184492791314751837736226655433438672131324422323339645329122493249927796152578575586385248714194662666383519" <>
    "52867629471723841866674395163672193918237743386921519264727173732356129118487733877712441449691494824775194378228634226621574619684442819723531496955154949925379274921113881938375538446717192914824423377613" <>
    "21272333982924289323437277224565149928416255435841327756139118119744528993269157174414264387573331116323982614862952264597611885999285995516357519648695594299657387614793341626318866519144574571816535351149" <>
    "394735916975448425618171572917195165594323552199346814729617189679698944337146"
    1228 = Day1.solve(data)
    1238 = Day1.solve2(data)
  end

  test "Real solutions for day 2" do
    matrix = "1224	926	1380	688	845	109	118	88	1275	1306	91	796	102	1361	27	995
    1928	2097	138	1824  198	117	1532	2000	1478	539	1982	125	1856	139	475	1338
    848	202	1116	791	1114	236	183	186	150	1016	1258	84	952	1202	988	866
    946	155	210	980	896	875	925	613	209	746	147	170	577	942	475	850
    1500	322	43	95	74	210	1817	1631	1762	128	181	716	171	1740	145	1123
    3074	827	117	2509	161	206	2739	253	2884	248	3307	2760	2239	1676	1137	3055
    183	85	143	197	243	72	291	279	99	189	30	101	211	209	77	198
    175	149	259	372	140	250	168	142	146	284	273	74	162	112	78	29
    169	578	97	589	473	317	123	102	445	217	144	398	510	464	247	109
    3291	216	185	1214	167	495	1859	194	1030	3456	2021	1622	3511	222	3534	1580
    2066	2418	2324	93	1073	82	102	538	1552	962	91	836	1628	2154	2144	1378
    149	963	1242	849	726	1158	164	1134	658	161	1148	336	826	1303	811	178
    3421	1404	2360	2643	3186	3352	1112	171	168	177	146	1945	319	185	2927	2289
    543	462	111	459	107	353	2006	116	2528	56	2436	1539	1770	125	2697	2432
    1356	208	5013	4231	193	169	3152	2543	4430	4070	4031	145	4433	4187	4394	1754
    5278	113	4427	569	5167	175	192	3903	155	1051	4121	5140	2328	203	5653	3233"
    34581 = Day2.solve(matrix)
    214 = Day2.solve2(matrix)
  end

  test "Real solutions for day3" do
    419 = Day3.solve(289326)
    295229 = Day3.solve2(289326)
  end

  test "Real solutions for day4" do
    {:ok, passwords} = File.read("priv/day4data") 
    386 = Day4.solve(passwords)
    208 = Day4.solve2(passwords)
  end

  test "Real solutions for Day 5" do
    {:ok, data} = File.read("priv/day5data") 
    339351 = Day5.solve(data)
    24315397 = Day5.solve2(data)
  end

  test "Real solutions for Day 6" do
    data = "4	1	15	12	0	9	9	5	5	8	7	3	14	5	12	3"
    6681 = Day6.solve(data)
    2392 = Day6.solve2(data)
  end

  test "Real solutions for Day 7" do
    {:ok, input} = File.read("priv/day7data") 
    :eqgvf = Day7.solve(input)
    757 = Day7.solve2(input)
  end

  test "Real solutions for Day 8" do
    {:ok, input} = File.read("priv/day8data") 
    5966 = Day8.solve(input)
    6347 = Day8.solve2(input)
  end

  test "Real solutions for Day 9" do
    {:ok, input} = File.read("priv/day9data") 
    17390 = Day9.solve(input)
    7825 = Day9.solve2(input)
  end

  test "Real solutions for Day 10" do
    data = "129,154,49,198,200,133,97,254,41,6,2,1,255,0,191,108"
    19591 = Day10.solve(data, 0..255)
    "62e2204d2ca4f4924f6e7a80f1288786" = Day10.solve2(data)
  end
  
  test "Real solutions for Day 11" do
    {:ok, input} = File.read("priv/day11data")
    818 = Day11.solve(input)
    1596 = Day11.solve2(input)
  end
  
  test "Real solutions for Day 12" do
    {:ok, input} = File.read("priv/day12data")
    169 = Day12.solve(input)
    179 = Day12.solve2(input)
  end
  
  test "Real solutions for Day 13" do
    {:ok, input} = File.read("priv/day13data")
    3184 = Day13.solve(input)
    3878062 = Day13.solve2(input)
  end
  
  test "Real solutions for Day 14" do
    input = "uugsqrei"
    8194 = Day14.solve(input)
    1141 = Day14.solve2(input)
  end
  
  test "Real solutions for Day 15" do
    567 = Day15.solve(40000000, 512, 191)
    323 = Day15.solve2(5000000, 512, 191)
  end
  
  test "Real solutions for Day 16" do
    {:ok, input} = File.read("priv/day16data")
    "lgpkniodmjacfbeh" = Day16.solve(input)
    "hklecbpnjigoafmd" = Day16.solve2(input)
  end
  
  test "Real solutions for Day 17" do
    input = 355
    1912 = Day17.solve(input)
    21066990 = Day17.solve2(input)
  end
  
  test "Real solutions for Day 18" do
    {:ok, input} = File.read("priv/day18data")
    2951 = Day18.solve(input)
    7366 = Day18.solve2(input)
  end
  
  test "Real solutions for Day 19" do
    {:ok, input} = File.read("priv/day19data")
    "MKXOIHZNBL" = Day19.solve(input)
    17872 = Day19.solve2(input)
  end
end
