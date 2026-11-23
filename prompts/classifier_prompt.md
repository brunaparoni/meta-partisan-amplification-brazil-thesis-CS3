# Political-content classifier: prompt

Prompt used to annotate URLs and as the system prompt for the fine-tuned classifier
(`gpt-4o-mini-2024-07-18`, suffix `br_url_politics_classifier_v1`). Labels: `politics` / `not politics`.

---

**Task:** Classify each entry as "politics" or "not politics".

**What is each entry:** Each single entry contains an URL title and description grouped.

**Warnings:** Don't open the URL - the classification should consider the title and description only.

**Criteria:**

- Classify as "politics":
  - entries with explicit references to political topics;
  - entries with content that could be politicized or used for political purposes;

  Consider as political any content that could influence public opinion or discussions about societal and public
  matters. This includes formal politics, politicians actions, public policies, societal debates, or any topic that
  could engage individuals in discussions about governance, societal norms, or collective issues. Examples might
  include, but are not limited to: elections, government actions, policies, legal reforms, societal rights and
  movements, scientific funding debates, or public controversies.

- Classify as "not politics":
  - any NA values;
  - entries in languages other than Portuguese;
  - entries that are too brief for adequate evaluation;
  - entries unrelated to political or societal debates, such as general entertainment, routine sports news,
    lifestyle, or anything without broader societal impact.
