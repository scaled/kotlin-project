//
// Scaled Kotlin Project Support - Kotlin project support for Scaled project framework.
// http://github.com/scaled/kotlin-project/blob/master/LICENSE

package scaled.project

import codex.extract.Extractor
import codex.extract.TokenExtractor
import scaled._

@Plugin(tag="codex-extractor")
class KotlinExtractorPlugin extends ExtractorPlugin {

  override val suffs = Set("kt")

  override def extractor (project :Project, suff :String) = Some(new TokenExtractor())
}
