# try to create a S4 object: Member to make it easier to add members to existing pedigrees
test1 <- SimPed()
# Load the methods package (necessary for S4 classes)
library(methods)

# Define the FamilyMember class (as previously described)
setClass(
      "Member",
      representation(
            fam = "numeric",
            ID = "numeric",
            gen = "numeric",
            dadID = "numeric",
            momID = "numeric",
            spt = "numeric",
            sex = "numeric"
      ),
      prototype(
            fam = NA_real_,
            ID = NA_real_,
            gen = NA_real_,
            dadID = NA_real_,
            momID = NA_real_,
            spt = NA_real_,
            sex = NA_real_
      )
)

# Constructor function for FamilyMember class
Member <- function(ID, fam = NA, gen = NA, dadID = NA, momID = NA, spt = NA, sex = NA) {
      member <- new("FamilyMember",
                    fam = fam,
                    ID = ID,
                    gen = gen,
                    dadID = dadID,
                    momID = momID,
                    spt = spt,
                    sex = sex)
      return(member)
}

# Define the Pedigree class
setClass(
      "Pedigree",
      representation(
            members = "list",
            G = "numeric",
            k = "numeric"
      ),
      prototype(
            members = list(),
            G = NA_real_,
            k = NA_real_
      )
)

# Constructor function for Pedigree class
Pedigree <- function(members, G, k) {
      pedigree <- new("Pedigree",
                      members = members,
                      G = G,
                      k = k)
      return(pedigree)
}

# Example usage
test_member1 <- Member(id = 1, father_id = 2, mother_id = 3, spouse_id = 4)
test_member2 <- Member(id = 5, father_id = 6, mother_id = 7, spouse_id = 8)

pedigree <- Pedigree(
      family_members = list(family_member1, family_member2),
      total_generations = 3,
      avg_offspring_per_couple = 2.5
)

print(pedigree)
