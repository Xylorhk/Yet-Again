using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TriggerIdle : MonoBehaviour
{
    [SerializeField] public Animator atlasController;


    private void OnTriggerEnter(Collider other)
    {
        if (other.CompareTag("Atlas"))
        {
            atlasController.SetTrigger("EnteredVolume");
        }
    }
}
