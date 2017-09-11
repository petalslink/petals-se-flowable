/**
 * Copyright (c) 2014-2017 Linagora
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

import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_SERVICE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_TASK_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_TASK_SERVICE;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URL;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;

import org.junit.ClassRule;
import org.junit.rules.RuleChain;
import org.junit.rules.TestRule;
import org.ow2.petals.component.framework.junit.helpers.SimpleComponent;
import org.ow2.petals.component.framework.junit.impl.ConsumesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.ComponentUnderTest;
import org.ow2.petals.component.framework.junit.rule.NativeServiceConfigurationFactory;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;
import org.ow2.petals.components.flowable.generic._1.ActivateProcessInstances;
import org.ow2.petals.components.flowable.generic._1.ActivateProcessInstancesResponse;
import org.ow2.petals.components.flowable.generic._1.GetTasks;
import org.ow2.petals.components.flowable.generic._1.GetTasksResponse;
import org.ow2.petals.components.flowable.generic._1.SuspendProcessInstances;
import org.ow2.petals.components.flowable.generic._1.SuspendProcessInstancesResponse;
import org.ow2.petals.se_flowable.unit_test.call_activity.archivageservice.Archiver;
import org.ow2.petals.se_flowable.unit_test.call_activity.archivageservice.ArchiverResponse;
import org.ow2.petals.se_flowable.unit_test.call_activity.archivageservice.UnknownDocument;
import org.ow2.petals.se_flowable.unit_test.call_activity.coreservice.Execute;
import org.ow2.petals.se_flowable.unit_test.call_activity.coreservice.ExecuteResponse;
import org.ow2.petals.se_flowable.unit_test.call_activity.level1.Start;
import org.ow2.petals.se_flowable.unit_test.call_activity.level1.StartResponse;
import org.ow2.petals.se_flowable.unit_test.call_activity.level1.Unlock;
import org.ow2.petals.se_flowable.unit_test.call_activity.level1.UnlockAck;

import com.ebmwebsourcing.easycommons.lang.UncheckedException;

/**
 * Abstract class for unit tests about request processing
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public abstract class CallActivityProcessTestEnvironment extends AbstractTestEnvironment {

    protected static final String CALL_ACTIVITY_SU = "call-activity-su";

    protected static final String CALL_ACTIVITY_PROVIDER_SU = "call-activity-provider-su";

    private static final String CALL_ACTIVITY_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/call-activity/level1";

    protected static final QName CALL_ACTIVITY_INTERFACE = new QName(CALL_ACTIVITY_NAMESPACE, "call-activity");

    protected static final QName CALL_ACTIVITY_SERVICE = new QName(CALL_ACTIVITY_NAMESPACE, "call-activity-service");

    protected static final String CALL_ACTIVITY_ENDPOINT = "edpCallActivity";

    protected static final QName OPERATION_START = new QName(CALL_ACTIVITY_NAMESPACE, "start");

    protected static final QName OPERATION_UNLOCK = new QName(CALL_ACTIVITY_NAMESPACE, "unlock");

    protected static final String BPMN_PROCESS_DEFINITION_KEY = "processLevel1";

    protected static final String BPMN_USER = "kermit";

    private static final String ARCHIVE_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/call-activity/archivageService";

    protected static final QName ARCHIVE_INTERFACE = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final QName ARCHIVE_SERVICE = new QName(ARCHIVE_NAMESPACE, "archiverService");

    protected static final String ARCHIVE_ENDPOINT = "archiveEndpointName";

    protected static final QName ARCHIVER_OPERATION = new QName(ARCHIVE_NAMESPACE, "archiver");

    private static final String CORE_SVC_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/call-activity/coreService";

    protected static final QName CORE_SVC_INTERFACE = new QName(CORE_SVC_NAMESPACE, "core");

    protected static final QName CORE_SVC_SERVICE = new QName(CORE_SVC_NAMESPACE, "coreService");

    protected static final String CORE_SVC_ENDPOINT = "coreServiceEndpointName";

    protected static final QName CORE_SVC_OPERATION = new QName(CORE_SVC_NAMESPACE, "execute");

    protected static final ComponentUnderTest COMPONENT_UNDER_TEST = new ComponentUnderTest()
            .addLogHandler(IN_MEMORY_LOG_HANDLER.getHandler())
            // A async job executor is required to process service task
            .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_ENABLE_JOB_EXECUTOR),
                    Boolean.TRUE.toString())
            .registerServiceToDeploy(CALL_ACTIVITY_SU, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final ServiceConfiguration serviceConfiguration = new ServiceConfiguration();

                    final URL bpmnUrl_3 = Thread.currentThread().getContextClassLoader()
                            .getResource("su/call-activity/Process-Level-3.bpmn");
                    assertNotNull("BPMN file not found", bpmnUrl_3);
                    serviceConfiguration.addResource(bpmnUrl_3);

                    serviceConfiguration.setServicesSectionParameter(
                            new QName(FlowableSEConstants.NAMESPACE_SU, "process_file"), "Process-Level-3.bpmn");
                    serviceConfiguration
                            .setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "version"), "1");

                    // Consume service 'core'
                    final ConsumesServiceConfiguration consumeServiceConfiguration = new ConsumesServiceConfiguration(
                            CORE_SVC_INTERFACE, CORE_SVC_SERVICE, CORE_SVC_ENDPOINT);
                    serviceConfiguration.addServiceConfigurationDependency(consumeServiceConfiguration);

                    return serviceConfiguration;
                }
            }).registerServiceToDeploy(CALL_ACTIVITY_PROVIDER_SU, new ServiceConfigurationFactory() {
                @Override
                public ServiceConfiguration create() {

                    final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/call-activity/call-activity.wsdl");
                    assertNotNull("WSDl not found", wsdlUrl);
                    final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                            CALL_ACTIVITY_INTERFACE, CALL_ACTIVITY_SERVICE, CALL_ACTIVITY_ENDPOINT, wsdlUrl);

                    final URL startResponseXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/call-activity/startResponse.xsl");
                    assertNotNull("Output XSL 'startResponse.xsl' not found", startResponseXslUrl);
                    serviceConfiguration.addResource(startResponseXslUrl);

                    final URL unlockResponseXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/call-activity/unlockAckResponse.xsl");
                    assertNotNull("Output XSL 'unlockAckResponse.xsl' not found", unlockResponseXslUrl);
                    serviceConfiguration.addResource(unlockResponseXslUrl);

                    final URL alreadyUnlockedXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/call-activity/alreadyUnlocked.xsl");
                    assertNotNull("Output XSL 'alreadyUnlocked.xsl' not found", alreadyUnlockedXslUrl);
                    serviceConfiguration.addResource(alreadyUnlockedXslUrl);

                    final URL instanceUnknownXslUrl = Thread.currentThread().getContextClassLoader()
                            .getResource("su/call-activity/instanceUnknown.xsl");
                    assertNotNull("Output XSL 'instanceUnknown.xsl' not found", instanceUnknownXslUrl);
                    serviceConfiguration.addResource(instanceUnknownXslUrl);

                    final URL bpmnUrl_1 = Thread.currentThread().getContextClassLoader()
                            .getResource("su/call-activity/Process-Level-1.bpmn");
                    assertNotNull("BPMN file not found", bpmnUrl_1);
                    serviceConfiguration.addResource(bpmnUrl_1);

                    serviceConfiguration.setServicesSectionParameter(
                            new QName(FlowableSEConstants.NAMESPACE_SU, "process_file1"), "Process-Level-1.bpmn");
                    serviceConfiguration
                            .setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "version1"), "1");

                    final URL bpmnUrl_2 = Thread.currentThread().getContextClassLoader()
                            .getResource("su/call-activity/Process-Level-2.bpmn");
                    assertNotNull("BPMN file not found", bpmnUrl_2);
                    serviceConfiguration.addResource(bpmnUrl_2);

                    serviceConfiguration.setServicesSectionParameter(
                            new QName(FlowableSEConstants.NAMESPACE_SU, "process_file2"), "Process-Level-2.bpmn");
                    serviceConfiguration
                            .setServicesSectionParameter(new QName(FlowableSEConstants.NAMESPACE_SU, "version2"), "1");

                    final URL bpmnUrl_3 = Thread.currentThread().getContextClassLoader()
                            .getResource("su/call-activity/Process-Level-3.bpmn");
                    assertNotNull("BPMN file not found", bpmnUrl_3);
                    serviceConfiguration.addResource(bpmnUrl_3);

                    // Consume service 'archiver'
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
            }).registerExternalServiceProvider(ARCHIVE_ENDPOINT, ARCHIVE_SERVICE, ARCHIVE_INTERFACE)
            .registerExternalServiceProvider(CORE_SVC_ENDPOINT, CORE_SVC_SERVICE, CORE_SVC_INTERFACE);

    @ClassRule
    public static final TestRule chain = RuleChain.outerRule(TEMP_FOLDER).around(IN_MEMORY_LOG_HANDLER)
            .around(COMPONENT_UNDER_TEST);

    protected static final SimpleComponent COMPONENT = new SimpleComponent(COMPONENT_UNDER_TEST);

    protected static Marshaller MARSHALLER;

    protected static Unmarshaller UNMARSHALLER;

    static {
        try {
            final JAXBContext context = JAXBContext.newInstance(Archiver.class, ArchiverResponse.class,
                    UnknownDocument.class, Start.class, StartResponse.class, Unlock.class, UnlockAck.class,
                    Execute.class, ExecuteResponse.class, SuspendProcessInstances.class,
                    SuspendProcessInstancesResponse.class, ActivateProcessInstances.class,
                    ActivateProcessInstancesResponse.class, GetTasks.class, GetTasksResponse.class);
            UNMARSHALLER = context.createUnmarshaller();
            MARSHALLER = context.createMarshaller();
            MARSHALLER.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
        } catch (final JAXBException e) {
            throw new UncheckedException(e);
        }
    }

    @Override
    protected ComponentUnderTest getComponentUnderTest() {
        return COMPONENT_UNDER_TEST;
    }

    /**
     * Convert a JAXB element to bytes
     * 
     * @param jaxbElement
     *            The JAXB element to write as bytes
     */
    protected static byte[] toByteArray(final Object jaxbElement) throws JAXBException, IOException {

        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            MARSHALLER.marshal(jaxbElement, baos);
            return baos.toByteArray();
        } finally {
            baos.close();
        }
    }
}
