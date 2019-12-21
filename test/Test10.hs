module Test10 (
  test10
) where

import Test.HUnit
import Day10

map0 = "###\n###\n#.#"
map1 = ".#..#\n.....\n#####\n....#\n...##"
map2 = "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"
map3 = "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###."
map4 = ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#.."
map5 = ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##"
map6 = ".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....#...###..\n..#.#.....#....##"

select :: [Int] -> [a] -> [a]
select is xs = map (xs !!) is

test10 = test [
  "count-map1" ~: maxVisibleCount (parseMap map1) ~?= 8,
  "count-map2" ~: maxVisibleCount (parseMap map2) ~?= 33,
  "count-map3" ~: maxVisibleCount (parseMap map3) ~?= 35,
  "count-map4" ~: maxVisibleCount (parseMap map4) ~?= 41,
  "count-map5" ~: maxVisibleCount (parseMap map5) ~?= 210,
  "destruction-map5" ~: select [0,1,2,9,19,49,99,198,199,200,298] (destructionList (11,13) (parseMap map5)) ~?= [(11,12),(12,1),(12,2),(12,8),(16,0),(16,9),(10,16),(9,6),(8,2),(10,9),(11,1)],
  "destruction-map0" ~: (destructionList (1,1) (parseMap map0)) ~?= [(1,0),(2,0),(2,1),(2,2),(0,2),(0,1),(0,0)],
  "destruction-map6" ~: take 4 (destructionList (8,3) (parseMap map6)) ~?= [(8,1),(9,0),(9,1),(10,0)]
  ]