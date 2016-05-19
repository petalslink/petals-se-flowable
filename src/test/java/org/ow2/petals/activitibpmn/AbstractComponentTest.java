/**
 * Copyright (c) 2014-2016 Linagora
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
package org.ow2.petals.activitibpmn;

import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DEFAULT_JDBC_URL_DATABASE_FILENAME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_PORT_TYPE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_SERVICE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_TASK_PORT_TYPE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_TASK_SERVICE;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;

import org.activiti.engine.history.HistoricActivityInstanceQuery;
import org.activiti.engine.history.HistoricProcessInstance;
import org.activiti.engine.history.HistoricProcessInstanceQuery;
import org.activiti.engine.history.HistoricTaskInstance;
import org.activiti.engine.history.HistoricTaskInstanceQuery;
import org.activiti.engine.runtime.ProcessInstance;
import org.activiti.engine.runtime.ProcessInstanceQuery;
import org.activiti.engine.task.Task;
import org.activiti.engine.task.TaskQuery;
import org.junit.Before;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.rules.RuleChain;
import org.junit.rules.TemporaryFolder;
import org.junit.rules.TestRule;
import org.ow2.petals.activitibpmn.junit.ActivitiClient;
import org.ow2.petals.component.framework.junit.helpers.SimpleComponent;
import org.ow2.petals.component.framework.junit.impl.ConsumesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.ComponentUnderTest;
import org.ow2.petals.component.framework.junit.rule.NativeServiceConfigurationFactory;
import org.ow2.petals.component.framework.junit.rule.ParameterGenerator;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;
import org.ow2.petals.components.activiti.generic._1.ActivateProcessInstances;
import org.ow2.petals.components.activiti.generic._1.ActivateProcessInstancesResponse;
import org.ow2.petals.components.activiti.generic._1.GetProcessInstances;
import org.ow2.petals.components.activiti.generic._1.GetProcessInstancesResponse;
import org.ow2.petals.components.activiti.generic._1.GetTasks;
import org.ow2.petals.components.activiti.generic._1.GetTasksResponse;
import org.ow2.petals.components.activiti.generic._1.InvalidRequest;
import org.ow2.petals.components.activiti.generic._1.SuspendProcessInstances;
import org.ow2.petals.components.activiti.generic._1.SuspendProcessInstancesResponse;
import org.ow2.petals.junit.rules.log.handler.InMemoryLogHandler;
import org.ow2.petals.samples.se_bpmn.archivageservice.Archiver;
import org.ow2.petals.samples.se_bpmn.archivageservice.ArchiverResponse;
import org.ow2.petals.samples.se_bpmn.vacationservice.AckResponse;
import org.ow2.petals.samples.se_bpmn.vacationservice.Demande;
import org.ow2.petals.samples.se_bpmn.vacationservice.DemandeDejaValidee;
import org.ow2.petals.samples.se_bpmn.vacationservice.JiraPETALSSEACTIVITI4;
import org.ow2.petals.samples.se_bpmn.vacationservice.Numero;
import org.ow2.petals.samples.se_bpmn.vacationservice.NumeroDemandeInconnu;
import org.ow2.petals.samples.se_bpmn.vacationservice.Validation;

import com.ebmwebsourcing.easycommons.lang.UncheckedException;

/**
 * Abstract class for unit tests about request processing
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public abstract class AbstractComponentTest extends AbstractTest {

    private static final String VACATION_NAMESPACE = "http://petals.ow2.org/samples/se-bpmn/vacationService";

    protected static final QName VACATION_INTERFACE = new QName(VACATION_NAMESPACE, "demandeDeConges");

    protected static final QName VACATION_SERVICE = new QName(VACATION_NAMESPACE, "demandeDeCongesService");

    protected static final String VACATION_ENDPOINT = "testEndpointName";

    protected static final QName OPERATION_JIRA = new QName(VACATION_NAMESPACE, "jira_PETALSSEACTIVITI-4");

    protected static final QName OPERATION_DEMANDERCONGES = new QName(VACATION_NAMESPACE, "demanderConges");

    protected static final QName OPERATION_VALIDERDEMANDE = new QName(VACATION_NAMESPACE, "validerDemande");

    private static final String ARCHIVE_NAMESPACE = "http://petals.ow2.org/samples/se-bpmn/archivageService";

    protected static final QName ARCHIVE_INTERFACE = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final QName ARCHIVE_SERVICE = new QName(ARCHIVE_NAMESPACE, "archiverService");

    protected static final String ARCHIVE_ENDPOINT = "archiveEndpointName";

    protected static final QName ARCHIVER_OPERATION = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final String VALID_SU = "valid-su";

    protected static final String NATIVE_TASKS_SVC_CFG = "native-tasks";

    protected static final String NATIVE_PROCESSINSTANCES_SVC_CFG = "native-process-instances";

    protected static final String BPMN_PROCESS_DEFINITION_KEY = "vacationRequest";

    protected static final String BPMN_PROCESS_1ST_USER_TASK_KEY = "handleRequest";

    protected static final String BPMN_PROCESS_2ND_USER_TASK_KEY = "adjustVacationRequestTask";

    protected static final String BPMN_USER_DEMANDEUR = "demandeur";

    protected static final String BPMN_USER_VALIDEUR = "valideur";

    protected static final InMemoryLogHandler IN_MEMORY_LOG_HANDLER = new InMemoryLogHandler();

    private static final TemporaryFolder TEMP_FOLDER = new TemporaryFolder();

    protected static final ComponentUnderTest COMPONENT_UNDER_TEST = new ComponentUnderTest()
            .addLogHandler(IN_MEMORY_LOG_HANDLER.getHandler())
            // A async job executor is required to process service task
            .setParameter(
                    new QName(ActivitiSEConstants.NAMESPACE_COMP, ActivitiSEConstants.ENGINE_ENABLE_JOB_EXECUTOR),
                    Boolean.TRUE.toString())
            .setParameter(
                    new QName(ActivitiSEConstants.NAMESPACE_COMP, ActivitiSEConstants.ENGINE_IDENTITY_SERVICE_CFG_FILE),
                    // Generate identity service configuration files
                    new ParameterGenerator() {

                        @Override
                        public String generate() throws Exception {
                            final URL identityServiceCfg = Thread.currentThread().getContextClassLoader()
                                    .getResource("su/valid/identityService.properties");
                            assertNotNull("Identity service config file is missing !", identityServiceCfg);
                            return new File(identityServiceCfg.toURI()).getAbsolutePath();
                        }

                    })
            .registerServiceToDeploy(VALID_SU, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/valid/vacationRequest.wsdl");
                    assertNotNull("WSDl not found", wsdlUrl);
                    final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                            VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT, wsdlUrl);

                    final URL demanderCongesResponseXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/valid/demanderCongesResponse.xsl");
                    assertNotNull("Output XSL 'demanderCongesResponse.xsl' not found", demanderCongesResponseXslUrl);
                    serviceConfiguration.addResource(demanderCongesResponseXslUrl);

                    final URL validerDemandeResponseXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/valid/validerDemandeResponse.xsl");
                    assertNotNull("Output XSL 'validerDemandeResponse.xsl' not found", validerDemandeResponseXslUrl);
                    serviceConfiguration.addResource(validerDemandeResponseXslUrl);

                    final URL ajusterDemandeResponseXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/valid/ajusterDemandeResponse.xsl");
                    assertNotNull("Output XSL 'ajusterDemandeResponse.xsl' not found", ajusterDemandeResponseXslUrl);
                    serviceConfiguration.addResource(ajusterDemandeResponseXslUrl);

                    final URL numeroDemandeInconnuXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/valid/numeroDemandeInconnu.xsl");
                    assertNotNull("Output XSL 'numeroDemandeInconnu.xsl' not found", numeroDemandeInconnuXslUrl);
                    serviceConfiguration.addResource(numeroDemandeInconnuXslUrl);

                    final URL demandeDejaValideeXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/valid/demandeDejaValidee.xsl");
                    assertNotNull("Output XSL 'demandeDejaValidee.xsl' not found", demandeDejaValideeXslUrl);
                    serviceConfiguration.addResource(demandeDejaValideeXslUrl);

                    final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/valid/vacationRequest.bpmn20.xml");
                    assertNotNull("BPMN file not found", bpmnUrl);
                    serviceConfiguration.addResource(bpmnUrl);

                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU, "process_file"),
                            "vacationRequest.bpmn20.xml");
                    serviceConfiguration.setParameter(new QName(ActivitiSEConstants.NAMESPACE_SU, "version"), "1");

                    // Consume service 'archiver'
                    // TODO: The consume section seems mandatory to retrieve the consume endpoint on async exchange
                    // between Activiti and other services
                    final ConsumesServiceConfiguration consumeServiceConfiguration = new ConsumesServiceConfiguration(
                            ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT);
                    serviceConfiguration.addServiceConfigurationDependency(consumeServiceConfiguration);

                    return serviceConfiguration;
                }
            }).registerNativeServiceToDeploy(NATIVE_TASKS_SVC_CFG, new NativeServiceConfigurationFactory() {

                @Override
                public ServiceConfiguration create(final String nativeEndpointName) {

                    final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("component.wsdl");
                    assertNotNull("Integration servce WSDl not found", nativeServiceWsdlUrl);
                    return new ProvidesServiceConfiguration(ITG_TASK_PORT_TYPE, ITG_TASK_SERVICE, nativeEndpointName,
                            nativeServiceWsdlUrl);
                }

                @Override
                public QName getNativeService() {
                    return ITG_TASK_SERVICE;
                }
            }).registerNativeServiceToDeploy(NATIVE_PROCESSINSTANCES_SVC_CFG, new NativeServiceConfigurationFactory() {

                @Override
                public ServiceConfiguration create(final String nativeEndpointName) {

                    final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("component.wsdl");
                    assertNotNull("Integration servce WSDl not found", nativeServiceWsdlUrl);
                    return new ProvidesServiceConfiguration(ITG_PROCESSINSTANCES_PORT_TYPE,
                            ITG_PROCESSINSTANCES_SERVICE, nativeEndpointName, nativeServiceWsdlUrl);
                }

                @Override
                public QName getNativeService() {
                    return ITG_PROCESSINSTANCES_SERVICE;
                }
            }).registerExternalServiceProvider(ARCHIVE_ENDPOINT, ARCHIVE_SERVICE, ARCHIVE_INTERFACE);

    private static Marshaller MARSHALLER;

    protected static Unmarshaller UNMARSHALLER;

    @ClassRule
    public static final TestRule chain = RuleChain.outerRule(TEMP_FOLDER).around(IN_MEMORY_LOG_HANDLER)
            .around(COMPONENT_UNDER_TEST);

    protected static final SimpleComponent COMPONENT = new SimpleComponent(COMPONENT_UNDER_TEST);

    @Rule
    public ActivitiClient activitiClient = new ActivitiClient(new File(new File(
            COMPONENT_UNDER_TEST.getBaseDirectory(), "work"), DEFAULT_JDBC_URL_DATABASE_FILENAME),
            "su/valid/identityService.properties");

    static {
        try {
            final JAXBContext context = JAXBContext.newInstance(Demande.class, Validation.class, Numero.class,
                    AckResponse.class, NumeroDemandeInconnu.class, DemandeDejaValidee.class, Archiver.class,
                    ArchiverResponse.class, GetTasks.class, GetTasksResponse.class, GetProcessInstances.class,
                    GetProcessInstancesResponse.class, JiraPETALSSEACTIVITI4.class, InvalidRequest.class,
                    SuspendProcessInstances.class, SuspendProcessInstancesResponse.class,
                    ActivateProcessInstances.class, ActivateProcessInstancesResponse.class);
            UNMARSHALLER = context.createUnmarshaller();
            MARSHALLER = context.createMarshaller();
            MARSHALLER.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
        } catch (final JAXBException e) {
            throw new UncheckedException(e);
        }
    }

    /**
     * All log traces must be cleared before starting a unit test
     */
    @Before
    public void clearLogTraces() {
        IN_MEMORY_LOG_HANDLER.clear();
    }

    /**
     * Convert a JAXB element to bytes
     * 
     * @param jaxbElement
     *            The JAXB element to write as bytes
     */
    protected byte[] toByteArray(final Object jaxbElement) throws JAXBException, IOException {

        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            MARSHALLER.marshal(jaxbElement, baos);
            return baos.toByteArray();
        } finally {
            baos.close();
        }
    }

    /**
     * Assertion about the pending state of a process instance. The process instance is not finished.
     * 
     * @param processInstanceId
     *            The process instance identifier
     * @param processDefinitionKey
     *            The process definition key
     */
    protected void assertProcessInstancePending(final String processInstanceId, final String processDefinitionKey) {

        final ProcessInstanceQuery processInstQuery = this.activitiClient.getRuntimeService()
                .createProcessInstanceQuery();
        final ProcessInstance processInstance = processInstQuery.processInstanceId(processInstanceId).singleResult();
        assertNotNull(processInstance);
        assertEquals(processInstanceId, processInstance.getProcessInstanceId());
        assertEquals(processDefinitionKey, processInstance.getProcessDefinitionKey());
        assertFalse(processInstance.isEnded());
        assertFalse(processInstance.isSuspended());
    }

    /**
     * @param processDefinitionKey
     *            The process definition identifier
     * @return The number of process instances that are not finished and associated to the given process definition
     */
    protected int getProcessInstanceNumber(final String processDefinitionKey) {
        final ProcessInstanceQuery processInstQuery = this.activitiClient.getRuntimeService()
                .createProcessInstanceQuery();
        final List<ProcessInstance> processInstances = processInstQuery.processDefinitionKey(processDefinitionKey)
                .list();
        assertNotNull(processInstances);
        return processInstances.size();
    }

    /**
     * Assertion about the creation of a process instance. The process instance is not finished.
     * 
     * @param processInstanceId
     *            The process instance identifier
     * @param processDefinitionKey
     *            The process definition key
     */
    protected void assertProcessInstanceFinished(final String processInstanceId) {

        final HistoricProcessInstanceQuery query = this.activitiClient.getHistoryService()
                .createHistoricProcessInstanceQuery();
        final HistoricProcessInstance processInstance = query.processInstanceId(processInstanceId).singleResult();
        assertNotNull(processInstance);
    }

    /**
     * Assertion about the current user task.
     * 
     * @param processInstanceId
     *            The process instance identifier
     * @param taskDefinitionKey
     *            The process definition key
     * @param user
     *            The task can be completed by the given user
     */
    protected void assertCurrentUserTask(final String processInstanceId, final String taskDefinitionKey,
            final String user) {

        final TaskQuery taskQuery = this.activitiClient.getTaskService().createTaskQuery();
        final Task nextTask = taskQuery.processInstanceId(processInstanceId).taskCandidateUser(user)
                .singleResult();
        assertNotNull(nextTask);
        assertEquals(processInstanceId, nextTask.getProcessInstanceId());
        assertEquals(taskDefinitionKey, nextTask.getTaskDefinitionKey());
    }

    /**
     * Assertion about a user task completed.
     * 
     * @param processInstanceId
     *            The process instance identifier
     * @param taskDefinitionKey
     *            The process definition key
     * @param user
     *            The task has been completed by the given user
     */
    protected void assertUserTaskEnded(final String processInstanceId, final String taskDefinitionKey, final String user) {

        final HistoricTaskInstanceQuery taskQuery = this.activitiClient.getHistoryService()
                .createHistoricTaskInstanceQuery();
        final HistoricTaskInstance nextTask = taskQuery.processInstanceId(processInstanceId)
                .taskDefinitionKey(taskDefinitionKey).singleResult();
        assertNotNull(nextTask);
        assertEquals(user, nextTask.getAssignee());
    }

    /**
     * Wait that a process instance ends.
     * 
     * @param processInstanceId
     *            The process instance identifier of the process instance to wait its end.
     */
    protected void waitEndOfProcessInstance(final String processInstanceId) throws InterruptedException {
        final CountDownLatch lock = new CountDownLatch(1);
        final Thread waitingThread = new Thread(new Runnable() {
            @Override
            public void run() {
                boolean run = true;
                try {
                    while (run) {
                        Thread.sleep(250);
                        final HistoricProcessInstanceQuery histProcInstQuery = AbstractComponentTest.this.activitiClient
                                .getHistoryService().createHistoricProcessInstanceQuery()
                                .processInstanceId(processInstanceId);
                        if (histProcInstQuery.singleResult() != null) {
                            // the process instance is finished
                            run = false;
                            lock.countDown();
                        }
                    }
                } catch (final InterruptedException e) {
                    e.printStackTrace();
                }
            }
        });
        waitingThread.start();
        lock.await(60, TimeUnit.SECONDS);
    }

    /**
     * Wait that a service task of a process instance ends.
     * 
     * @param processInstanceId
     *            The process instance identifier of the service task to wait its end.
     * @param taskDefinitionKey
     *            The service task identifier in the process definition
     */
    protected void waitEndOfServiceTask(final String processInstanceId, final String taskDefinitionKey)
            throws InterruptedException {
        final CountDownLatch lock = new CountDownLatch(1);
        final Thread waitingThread = new Thread(new Runnable() {
            @Override
            public void run() {
                boolean run = true;
                try {
                    while (run) {
                        Thread.sleep(250);
                        // Caution: service tasks are stored in activity historic, not in task historic.
                        final HistoricActivityInstanceQuery histSvcTaskQuery = AbstractComponentTest.this.activitiClient
                                .getHistoryService().createHistoricActivityInstanceQuery()
                                .processInstanceId(processInstanceId).activityId(taskDefinitionKey).finished();
                        if (histSvcTaskQuery.singleResult() != null) {
                            // the process instance is finished
                            run = false;
                            lock.countDown();
                        }
                    }
                } catch (final InterruptedException e) {
                    e.printStackTrace();
                }
            }
        });
        waitingThread.start();
        lock.await(60, TimeUnit.SECONDS);
    }

}
