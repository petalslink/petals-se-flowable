/**
 * Copyright (c) 2018-2025 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.GregorianCalendar;
import java.util.List;

import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.xml.transform.Source;

import org.flowable.engine.repository.ProcessDefinition;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.component.framework.junit.Message;
import org.ow2.petals.component.framework.junit.RequestMessage;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.StatusMessage;
import org.ow2.petals.component.framework.junit.helpers.ServiceProviderImplementation;
import org.ow2.petals.component.framework.junit.impl.message.RequestToProviderMessage;
import org.ow2.petals.component.framework.junit.impl.message.ResponseToConsumerMessage;
import org.ow2.petals.flowable.outgoing.WSDLImporterForFlowableFactory;
import org.ow2.petals.se_flowable.unit_test.vacation.archivageservice.Archiver;
import org.ow2.petals.se_flowable.unit_test.vacation.archivageservice.ArchiverResponse;
import org.ow2.petals.se_flowable.unit_test.vacation.vacationservice.AckResponse;
import org.ow2.petals.se_flowable.unit_test.vacation.vacationservice.Demande;
import org.ow2.petals.se_flowable.unit_test.vacation.vacationservice.Numero;
import org.ow2.petals.se_flowable.unit_test.vacation.vacationservice.Validation;

import com.ebmwebsourcing.easycommons.xml.SourceHelper;

/**
 * Unit tests about deployment of Flowable SU
 * 
 * @author Christophe DENEUX - Linagora
 */
public class DeploymentTest extends AbstractVacationProcessTestEnvironment {

    @BeforeAll
    private static void completesTestEnvConfiguration() throws Exception {
        completesComponentUnderTestConfiguration();
        completesFlowableClientConfiguration();
    }

    private static void completesFlowableClientConfiguration() throws Exception {
        FLOWABLE_CLIENT.start();

        // When purging all process definitions, WSDLs are imported. So we must use the right WSDL importer
        FLOWABLE_CLIENT.setXMLImporterFactory(
                new WSDLImporterForFlowableFactory(COMPONENT_UNDER_TEST.getComponentObject().getServiceUnitManager()));
    }

    private static void completesComponentUnderTestConfiguration() throws Exception {
        COMPONENT_UNDER_TEST
                .registerExternalServiceProvider(ARCHIVE_ENDPOINT, ARCHIVE_SERVICE, ARCHIVE_INTERFACE)
                .postInitComponentUnderTest();
    }

    @AfterEach
    public void undeployAllSus() {
        COMPONENT_UNDER_TEST.undeployAllServices();
    }

    @AfterEach
    public void undeployAllProcessDefinitons() {
        FLOWABLE_CLIENT.purge();
    }

    /**
     * <p>
     * Check the redeployment of a same service unit
     * </p>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs,</li>
     * <li>only one version of the process definition is deployed,</li>
     * <li>process instances started before redeployment continues correctly their execution after the
     * redeployment.</li>
     * </ul>
     */
    @Test
    public void redeploySameServiceUnit() throws Exception {

        // Validation of component state
        assertTrue(COMPONENT_UNDER_TEST.isInstalled());
        assertTrue(COMPONENT_UNDER_TEST.isStarted());

        // Validation of IDM engine configuration
        assertTrue(FLOWABLE_CLIENT.getIdentityService().checkPassword(BPMN_USER_DEMANDEUR, "demandeur"));
        assertEquals(BPMN_USER_DEMANDEUR, FLOWABLE_CLIENT.getIdentityService().createUserQuery()
                .memberOfGroup("employees").singleResult().getId());
        assertEquals(BPMN_USER_VALIDEUR, FLOWABLE_CLIENT.getIdentityService().createUserQuery()
                .memberOfGroup("management").singleResult().getId());
        assertEquals("employees", FLOWABLE_CLIENT.getIdentityService().createGroupQuery()
                .groupMember(BPMN_USER_DEMANDEUR).singleResult().getId());
        assertEquals("management", FLOWABLE_CLIENT.getIdentityService().createGroupQuery()
                .groupMember(BPMN_USER_VALIDEUR).singleResult().getId());

        // --------------------------------------------------------
        // ---- First deployment of the service unit
        // --------------------------------------------------------
        COMPONENT_UNDER_TEST.deployService(VACATION_SU, createVacationServiceCfgFactory());

        // Assert that only one version of the process is deployed
        final int currentProcessVersion;
        {
            final List<ProcessDefinition> deployments = FLOWABLE_CLIENT.getRepositoryService()
                    .createProcessDefinitionQuery().processDefinitionKey(BPMN_PROCESS_DEFINITION_KEY).list();
            assertNotNull(deployments);
            assertEquals(1, deployments.size());
            currentProcessVersion = deployments.get(0).getVersion();
        }

        // --------------------------------------------------------
        // ---- Create a new instance of the process definition
        // --------------------------------------------------------
        final String procInstId = this.createProcessInstance();

        // --------------------------------------------------------
        // ---- 2nd deployment of the same service unit
        // --------------------------------------------------------
        COMPONENT_UNDER_TEST.undeployService(VACATION_SU);
        COMPONENT_UNDER_TEST.deployService(VACATION_SU, createVacationServiceCfgFactory());

        // Assert that only one version of the process is deployed
        {
            final List<ProcessDefinition> deployments = FLOWABLE_CLIENT.getRepositoryService()
                    .createProcessDefinitionQuery().processDefinitionKey(BPMN_PROCESS_DEFINITION_KEY).list();
            assertNotNull(deployments);
            assertEquals(1, deployments.size(), "Unexpected number of process definitions");
            assertEquals(currentProcessVersion, deployments.get(0).getVersion(),
                    "Different version of the process definition");
        }

        // ---------------------------------------------------------------------------------------------------------------------------
        // ---- Complete the first user task (validate the vacation request) of the process instance started before the
        // ---- redeployment
        // ---------------------------------------------------------------------------------------------------------------------------
        this.completeFirstUserTask(procInstId);

        // Wait the end of a process instance
        this.waitEndOfProcessInstance(procInstId);

    }

    /**
     * <p>
     * Check the deployment of a new version of a service unit:
     * </p>
     * <ul>
     * <li>a first version of the service unit is deployed with a first version of process definition,</li>
     * <li>the first version of the service unit is undeployed,</li>
     * <li>a second version of the service unit is deployed with a second version of the process definition.</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs,</li>
     * <li>on first deployment, only one version of the process definition is deployed,</li>
     * <li>on undeployment, the first version of the process definition is always deployed,</li>
     * <li>on second deployment, a new version of the process definition is deployed,</li>
     * <li>process instances started before undeployment continues correctly their execution after the redeployment
     * because the new version has no change on service tasks,</li>
     * <li>after second deployment, new process instances are created from the second version of the process
     * definition.</li>
     * </ul>
     */
    @Test
    public void deployNewVersionServiceUnit() throws Exception {

        // Validation of component state
        assertTrue(COMPONENT_UNDER_TEST.isInstalled());
        assertTrue(COMPONENT_UNDER_TEST.isStarted());

        // Validation of IDM engine configuration
        assertTrue(FLOWABLE_CLIENT.getIdentityService().checkPassword(BPMN_USER_DEMANDEUR, "demandeur"));
        assertEquals(BPMN_USER_DEMANDEUR, FLOWABLE_CLIENT.getIdentityService().createUserQuery()
                .memberOfGroup("employees").singleResult().getId());
        assertEquals(BPMN_USER_VALIDEUR, FLOWABLE_CLIENT.getIdentityService().createUserQuery()
                .memberOfGroup("management").singleResult().getId());
        assertEquals("employees", FLOWABLE_CLIENT.getIdentityService().createGroupQuery()
                .groupMember(BPMN_USER_DEMANDEUR).singleResult().getId());
        assertEquals("management", FLOWABLE_CLIENT.getIdentityService().createGroupQuery()
                .groupMember(BPMN_USER_VALIDEUR).singleResult().getId());

        // --------------------------------------------------------
        // ---- Deployment of the first version of the service unit
        // --------------------------------------------------------
        final int firstBpmnVersion = 1;
        COMPONENT_UNDER_TEST.deployService(VACATION_SU, createVacationServiceCfgFactory(firstBpmnVersion));

        // Assert that only one version of the process is deployed
        {
            final List<ProcessDefinition> deployments = FLOWABLE_CLIENT.getRepositoryService()
                    .createProcessDefinitionQuery().processDefinitionKey(BPMN_PROCESS_DEFINITION_KEY).list();
            assertNotNull(deployments);
            assertEquals(1, deployments.size());
            assertEquals(firstBpmnVersion, deployments.get(0).getVersion());
        }

        // -------------------------------------------------------------------------
        // ---- Create a new instance of the first version of process definition
        // -------------------------------------------------------------------------
        final String procInstId_1 = this.createProcessInstance();

        // -----------------------------------------------------------
        // ---- Undeployment of the first version of the service unit
        // -----------------------------------------------------------
        COMPONENT_UNDER_TEST.undeployService(VACATION_SU);

        // Assert that the first version of the process is always deployed
        {
            final List<ProcessDefinition> deployments = FLOWABLE_CLIENT.getRepositoryService()
                    .createProcessDefinitionQuery().processDefinitionKey(BPMN_PROCESS_DEFINITION_KEY).list();
            assertNotNull(deployments);
            assertEquals(1, deployments.size(), "Unexpected number of process definitions");
            assertEquals(firstBpmnVersion, deployments.get(0).getVersion(),
                    "Different version of the process definition");
        }

        // ----------------------------------------------------------
        // ---- Deployment of the second version of the service unit
        // ----------------------------------------------------------
        final int secondBpmnVersion = 2;
        COMPONENT_UNDER_TEST.deployService(VACATION_SU, createVacationServiceCfgFactory(secondBpmnVersion));

        // Assert that the second version of the process is deployed
        {
            final ProcessDefinition deployment = FLOWABLE_CLIENT.getRepositoryService().createProcessDefinitionQuery()
                    .processDefinitionKey(BPMN_PROCESS_DEFINITION_KEY).processDefinitionVersion(secondBpmnVersion)
                    .singleResult();
            assertNotNull(deployment);
            assertEquals(secondBpmnVersion, deployment.getVersion());
        }

        // -------------------------------------------------------------------------
        // ---- Create a new instance of the second version of process definition
        // -------------------------------------------------------------------------
        final String procInstId_2 = this.createProcessInstance();

        // ---------------------------------------------------------------------------------------------------------------------------
        // ---- Complete the first user task (validate the vacation request) of the process instance started with the
        // first version of the process definition
        // ---------------------------------------------------------------------------------------------------------------------------
        this.completeFirstUserTask(procInstId_1);

        // ---------------------------------------------------------------------------------------------------------------------------
        // ---- Complete the first user task (validate the vacation request) of the process instance started with the
        // second version of the process definition
        // ---------------------------------------------------------------------------------------------------------------------------
        this.completeFirstUserTask(procInstId_2);

        // Wait the end of process instances
        this.waitEndOfProcessInstance(procInstId_1);
        this.waitEndOfProcessInstance(procInstId_2);

    }

    private String createProcessInstance() throws Exception {

        final Demande requestBean = new Demande();
        requestBean.setDemandeur(BPMN_USER_DEMANDEUR);
        final int numberOfDays = 10;
        requestBean.setNbJourDde(numberOfDays);
        final GregorianCalendar now = new GregorianCalendar();
        final GregorianCalendar startDate = new GregorianCalendar(now.get(GregorianCalendar.YEAR),
                now.get(GregorianCalendar.MONTH), now.get(GregorianCalendar.DAY_OF_MONTH));
        requestBean.setDateDebutDde(startDate.getTime());
        final String motivation = "hollidays from validStartEventRequest";
        requestBean.setMotifDde(motivation);

        // Send the 1st valid request for start event 'request
        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, VACATION_SU,
                OPERATION_DEMANDERCONGES, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(requestBean));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg = COMPONENT.sendAndGetResponse(request);

        // Check the reply
        final Source fault = responseMsg.getFault();
        assertNull(fault == null ? null : SourceHelper.toString(fault), "Unexpected fault");
        assertNotNull(responseMsg.getPayload(), "No XML payload in response");
        final Object responseObj = UNMARSHALLER.unmarshal(responseMsg.getPayload());
        assertInstanceOf(Numero.class, responseObj);
        final Numero response = (Numero) responseObj;
        assertNotNull(response.getNumeroDde());

        // Assertions about states of user task and process instance at Flowable Level
        this.assertProcessInstancePending(response.getNumeroDde(), BPMN_PROCESS_DEFINITION_KEY);
        this.assertCurrentUserTask(response.getNumeroDde(), BPMN_PROCESS_1ST_USER_TASK_KEY, BPMN_USER_VALIDEUR);

        return response.getNumeroDde();
    }

    private void completeFirstUserTask(final String procInstId) throws Exception {

        final Validation requestBean = new Validation();
        requestBean.setValideur(BPMN_USER_VALIDEUR);
        requestBean.setNumeroDde(procInstId);
        requestBean.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        final ServiceProviderImplementation service = new ServiceProviderImplementation() {
            private MessageExchange archiveMessageExchange;

            @Override
            public Message provides(final RequestMessage archiveRequestMsg) throws Exception {
                // Assert the 1st request sent by Flowable on orchestrated service
                this.archiveMessageExchange = archiveRequestMsg.getMessageExchange();
                assertNotNull(this.archiveMessageExchange);
                assertEquals(ARCHIVE_INTERFACE, this.archiveMessageExchange.getInterfaceName());
                assertEquals(ARCHIVE_SERVICE, this.archiveMessageExchange.getService());
                assertNotNull(this.archiveMessageExchange.getEndpoint());
                assertEquals(ARCHIVE_ENDPOINT, this.archiveMessageExchange.getEndpoint().getEndpointName());
                assertEquals(ARCHIVER_OPERATION, this.archiveMessageExchange.getOperation());
                assertEquals(ExchangeStatus.ACTIVE, this.archiveMessageExchange.getStatus());
                final Object archiveRequestObj = UNMARSHALLER.unmarshal(archiveRequestMsg.getPayload());
                assertInstanceOf(Archiver.class, archiveRequestObj);
                final Archiver archiveRequest = (Archiver) archiveRequestObj;
                assertEquals(procInstId, archiveRequest.getItem());

                // Returns the reply of the service provider to the Flowable service task
                final ArchiverResponse archiverResponse = new ArchiverResponse();
                archiverResponse.setItem("value of item");
                archiverResponse.setItem2("value of item2");
                return new ResponseToConsumerMessage(archiveRequestMsg, toByteArray(archiverResponse));
            }

            @Override
            public void handleStatus(final StatusMessage statusDoneMsg) throws Exception {
                // Assert the status DONE on the message exchange
                assertNotNull(statusDoneMsg);
                // It's the same message exchange instance
                assertSame(statusDoneMsg.getMessageExchange(), this.archiveMessageExchange);
                assertEquals(ExchangeStatus.DONE, statusDoneMsg.getMessageExchange().getStatus());
            }
        };

        final RequestToProviderMessage request = new RequestToProviderMessage(COMPONENT_UNDER_TEST, VACATION_SU,
                OPERATION_VALIDERDEMANDE, AbsItfOperation.MEPPatternConstants.IN_OUT.value(), toByteArray(requestBean));

        // Assert the response of the valid request
        final ResponseMessage responseMsg = COMPONENT.sendAndGetResponse(request, service);

        // Check the reply
        final Source fault = responseMsg.getFault();
        assertNull(fault == null ? null : SourceHelper.toString(fault), "Unexpected fault");
        assertNotNull(responseMsg.getPayload(), "No XML payload in response");
        final Object responseObj = UNMARSHALLER.unmarshal(responseMsg.getPayload());
        assertInstanceOf(AckResponse.class, responseObj);

        COMPONENT.sendDoneStatus(responseMsg, service);

    }
}
