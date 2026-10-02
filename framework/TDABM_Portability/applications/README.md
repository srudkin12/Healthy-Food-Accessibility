# Applications

This directory is intentionally empty in the clean baseline.

Create one subdirectory per independently auditable application/specification.

Never place application-specific source, labels, exclusions or paper claims in
`framework/`.

Use:

```bash
python3 tools/create_application_scaffold.py <application_id>
```

to create a new skeleton from `templates/new_application/`.
