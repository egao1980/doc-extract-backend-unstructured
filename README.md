# doc-extract-backend-unstructured

[`doc-extract-protocol`](https://github.com/egao1980/doc-extract-protocol) backend for the open-source [Unstructured API](https://github.com/Unstructured-IO/unstructured-api) container. Multipart POST to `/general/v0/general`. Unit tests inject `http-fn` and never need a live service.

```lisp
(asdf:load-system "doc-extract-backend-unstructured")

(let ((b (doc-extract-backend-unstructured:make-unstructured-backend
          :endpoint "http://127.0.0.1:8000"
          :strategy "auto"
          :http-fn (lambda (req) …))))
  (stack-doc-extract:extract-document b #p"memo.eml" :format :eml))
```

## API

| Binding | Role |
|---------|------|
| `unstructured-backend` | CLOS class (`doc-extract-backend`) |
| slots | `endpoint`, `strategy`, injectable `http-fn` |
| `make-unstructured-backend` | `&key endpoint strategy http-fn` |
| `use-unstructured-backend` | bind `*doc-extract-backend*` |
| `extract-document` | primary GF → `extracted-document` |
| `extract-sections` / `extract-text` / `extract-metadata` | protocol GFs |
| `normalize-document` | element JSON (string / list) → `extracted-document` |
| `make-curl-http-fn` | live canary helper (`curl` multipart POST) |

`http-fn` always receives an `unstructured-http-request` (`:post`, url, strategy, filename, content-type, octets) — including when `http-protocol` is loaded. Return a JSON string, octet vector, parsed element list, or `(status body)`. To send via `http-protocol:send`, wrap with `unstructured-request->http-request` (that helper passes file octets as `make-http-file`'s required positional `content`).

Default endpoint: `http://127.0.0.1:8000`. If the endpoint already includes `/general/v0/general`, it is not appended again. `strategy` is forwarded as a multipart field (`auto`, `fast`, `hi_res`, `ocr_only`).

## Element → document mapping

Unstructured returns a JSON list of elements. `extract-document` is the primary path:

- `Title` opens a `section`
- following `NarrativeText` / `ListItem` / `Table` nest under the last title
- consecutive `ListItem`s become one `list-block`
- `Table` uses `metadata.text_as_html` when present
- `Header` / `Footer` are `:furniture` at the document root
- `Image` → `image-block`; `CodeSnippet` / `Formula` → `code-block`

`extract-sections` is the 0.1 title/text/level view of those sections.

## Registry priority

`register-extractor` runs at load for the formats Unstructured covers (PDF, Office, HTML, email, EPUB, RTF, ODF, images, markdown, text, …).

| Backend | Default priority |
|---------|------------------|
| colocated HTML / DOCX / XLSX (`doc-extract-protocol`) | 10 |
| **this backend** (`+unstructured-priority+`) | **20** |
| planned `doc-extract-backend-pdf` (pdfium) | profile-set (corporate: below 20) |
| planned `doc-extract-backend-docling` | ~50 |

Corporate profile: docling > unstructured > pdfium. Personal profile should re-register this backend at priority **5** so colocated HTML/office and pdfium win on shared formats. Unique coverage (`.eml`, `.msg`, `.epub`, images, RTF, ODF, PPTX) still resolves here when nothing else is registered.

## Tests

`asdf:test-system "doc-extract-backend-unstructured"` is green without a container. Fixture: `tests/fixtures/elements.json`.

Live canary (skipped unless set):

```bash
export DOC_EXTRACT_UNSTRUCTURED_URL=http://127.0.0.1:8000
asdf:test-system "doc-extract-backend-unstructured"
```

Conditions: `doc-extract-error`, `unstructured-http-error`. Restarts: `use-value`, `retry`.

## License

MIT — see [LICENSE](LICENSE).
