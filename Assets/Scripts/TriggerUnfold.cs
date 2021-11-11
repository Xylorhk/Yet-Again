using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TriggerUnfold : MonoBehaviour
{
    [SerializeField] public Animator atlasController;


    private void OnTriggerEnter(Collider other)
    {
        if (other.CompareTag("Player"))
        {
            atlasController.SetTrigger("EnteredPlatform");
        }
    }
}
