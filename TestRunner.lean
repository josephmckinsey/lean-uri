import UriTesting.Parsing
import UriTesting.Resolution
import UriTesting.Normalization
import UriTesting.PublicAPI

def main (args : List String) : IO Unit := do
  let filter : String → Bool := match args with
  | [s] => fun groupName => groupName.startsWith s
  | _  => fun _ => true

  Testing.TestM.run (filter := filter) do
    Testing.group "Parsing Tests" do
      testPctEncoded
      testScheme
      testDecOctet
      testIPv4Address
      testIPv6Address
      testRegName
      testPort
      testHost
      testAuthority
      testPath
      testQuery
      testFragment
      testURI
    testRelativeResolution
    allNormalizationTests
    allPublicAPITests
    Testing.printSummary
